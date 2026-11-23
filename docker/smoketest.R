# PCAGO smoke test: runs the core analysis pipeline on bundled sample data.
# Usage (inside the server image):
#   docker run --rm --platform linux/amd64 -v "$(pwd)"/docker/smoketest.R:/tmp/smoketest.R:ro \
#     --entrypoint Rscript trichterhub/pcago-server:2.0-dev /tmp/smoketest.R
# Exits non-zero on the first failing step.

setwd("/srv/pcago")
suppressPackageStartupMessages(source("server.R"))

step <- function(name, expr) {
  t0 <- Sys.time()
  res <- tryCatch(force(expr), error = function(e) {
    message(sprintf("FAIL  %-35s %s", name, substr(conditionMessage(e), 1, 500)))
    quit(status = 1)
  })
  message(sprintf("OK    %-35s (%.1fs)", name, as.numeric(Sys.time() - t0, units = "secs")))
  invisible(res)
}

rc <- step("import read counts (Monocytes raw)",
           importReadcountSample("Monocytes/readcounts_rna.csv", list()))
readcounts <- rc$readcounts.raw  # importer returns a PCAGODataSet (R6)
stopifnot(is(readcounts, "SummarizedExperiment"))
message("      ", nrow(readcounts), " genes x ", ncol(readcounts), " samples")

readcounts <- step("remove zero reads", { r <- removeZeroReads(readcounts); if (is.list(r) && !is(r, "SummarizedExperiment")) r$readcounts else r })

dataset <- rc
dataset$readcounts.preprocessed <- readcounts

sa <- step("import sample annotation",
           importSampleAnnotationSample("Monocytes/sample_annotation_conditions.csv", dataset,
                                        list(imported_data = "conditions", collapse_conditions = F)))

ga <- step("import GFF gene annotation (txdbmaker)",
           importSampleGeneInformation("Monocytes/geneannotation.gff3", dataset,
                                       list(imported_data = c("sequence_info", "exon_length", "biotype", "scaffold"))))

conds <- colnames(sa@conditions)
deseq <- step("DESeq2 normalization",
              applyReadcountNormalization.DESeq2(readcounts, "none", F, sa, conds))
step("DESeq2 rlog transform",
     applyReadcountNormalization.DESeq2(readcounts[1:2000, ], "rlog", F, sa, conds))
step("TPM normalization (annotated genes)", {
  si <- ga@sequence.info
  annotated <- intersect(rownames(readcounts), rownames(si)[!is.na(si$exon_length)])
  stopifnot(length(annotated) > 100)
  applyReadcountNormalization.TPM(readcounts[annotated, ], sa, ga, use.feature.exonlength = T, use.feature.effectivelength = F)
})

normalized <- removeConstantReads(deseq$readcounts)
normalized <- if (is.list(normalized) && !is(normalized, "SummarizedExperiment")) normalized$readcounts else normalized
variances <- step("gene variance table", buildGeneVarianceTable(normalized))
top <- step("select top 500 variant genes", selectTopVariantGeneReadcounts(normalized, variances, 500))
top <- if (is.list(top) && !is(top, "SummarizedExperiment")) top$readcounts else top
pca <- step("PCA", applyPCA(top, center = T, scale = F, relative = "none"))

step("hierarchical clustering", {
  d <- dist(t(SummarizedExperiment::assay(top)))
  dendextend::as.ggdend(as.dendrogram(fastcluster::hclust(d)))
})
step("ggplot2 PCA plot to PNG (Cairo)", {
  df <- as.data.frame(pca$transformed)
  f <- tempfile(fileext = ".png")
  Cairo::CairoPNG(f, width = 800, height = 600)
  print(ggplot2::ggplot(df, ggplot2::aes(PC1, PC2)) + ggplot2::geom_point())
  dev.off()
  stopifnot(file.size(f) > 1000)
})
step("scatterplot3d + SVG export", {
  f <- tempfile(fileext = ".svg")
  svglite::svglite(f)
  scatterplot3d::scatterplot3d(pca$transformed[, 1:3])
  dev.off()
  stopifnot(file.size(f) > 1000)
})
step("VennDiagram", {
  f <- tempfile(fileext = ".png")
  Cairo::CairoPNG(f); grid::grid.draw(VennDiagram::venn.diagram(list(a = 1:10, b = 5:15), filename = NULL)); dev.off()
})
step("ffmpeg available", stopifnot(system2(ffmpeg.path, "-version", stdout = FALSE) == 0))
step("GO.db lookup", stopifnot(nrow(AnnotationDbi::select(GO.db::GO.db, keys = "GO:0008150", columns = "TERM", keytype = "GOID")) == 1))

message("ALL STEPS PASSED")
