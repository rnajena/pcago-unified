# Gene Ontology Browser

Due to the large amount of [Gene Ontology (GO)](http://www.geneontology.org/) terms,
the gene annotation based filtering requires pre-selecting the GO terms considered for filtering.
The GO Browser widget helps achieving this task by allowing to search for terms in the set of 
and browse the hierarchical structure of the GO terms.

![Screenshot of GO Browser](helppages/goBrowser.png)

## Navigating

By default, the GO browser displays the root terms of the three main categories of GO terms.
To browse the hierarchical structure, select the term of interest and click **List subterms**.
The list will then display all child terms of the selected one.

This will also update the *bread crumb bar* on top of the list, which tracks the position in the 
hierarchical structure. To quickly switch within the hierarchy, click the items in the bar.

To search for terms, type into the *Search GO term* field. This will hide the bread crumb bar.

**Tip:** You can list subterms from search results

## Adding filter items

To include one or multiple GO terms into the filter selection, select the terms in the list and
click **Add to filter**. To remove items, click **Remove from filter** within the *More ...* menu.
Navigating and searching GO terms does not disturb the list of considered terms.

## Looking up information

To show more information about one or more GO terms, select them in the list and click **Show details**.
This will open a dialog that contains information like a definition of the term and a link to the
matching [AmiGO 2](http://amigo.geneontology.org/) entry.

## Import and export

You can export your selected GO terms by clicking on **Export \*.csv**. The table will contain the GO terms and
additional info like the ID and a definition.

An imported table has following format:

| goid  | ... |
|------------|-----------------------------------------------------|
| GOID1 | ... |
| GOID2 | ...                                                 |
| GOID3 | ...                                                 |
| ...        | ...                                                 |

The importer looks only for the `goid` column which should contain valid GO accessions (usually in the format `GO:xxxxxxx`).
The other columns are ignored and can contain additional data.
