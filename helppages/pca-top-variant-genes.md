# Top variant genes

PCAGO allows to restrict the filtered gene set (see `PCA > Gene filtering` help page)
to only the top n most variant genes.

You can change this **n** in `Sidebar > PCA > Gene Count`.

## User interface

The gene count interface consists of following elements:

* **Plot** of the gene variances. The red marker line is at the current **n** (by default the max. gene count)
* **Slider** to control the gene count
* **Additional controls** that allow you to increase/decrease the current **n** by 1 or the *animation step*.
* **Play/Pause button** that controls the animation

## Animation

Below the controls you can find the *animation parameters* that allow you to set ...

* the **gene count range** of the slider and animation
* the **animation step** which decides by which step the current **n** is increased during the animation
* the **animation speed** that determines the amount of *ms* to wait after each plot. Smaller numbers increase the animation speed.

<div class="well help-box">
<label>Note</label> Due to the live rendering the animation may be not as smooth as a rendered video file.
The animation speed is additionally influenced by the server and network capabilities.
</div>

<div class="well help-box">
<label>Tip</label> Render a video file of the animation by clicking the <strong>Export *.mp4</strong> button on top of the plot.
</div>
