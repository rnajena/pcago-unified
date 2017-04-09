# Visual editor

For many plots you can change the colors, shape or name of the data points.
This is handled by the "Visuals" widget in the sidebar.

![Screenshot of visuals editor](helppages/visualEditor.png)

## User interface

The available conditions are listed in the "Available conditions" section.
The conditions are colored by the current *color* of the selected condition.
A bar at the left indicates that the condition sets a shape.

<div class="well help-box">
<label>Important</label> Depending on the plot you may have data points where <em>multiple conditions</em>
apply. The general rule is that the shape and color of a data point is chosen
from the <strong>left most</strong> condition in the list.
</div>

<div class="well help-box">
<label>Example</label> With the conditions as seen in the screenshot, a cell with conditions
atra, asp and eco gets the shape from atra and the color from asp.
The eco condition is ignored, because both color and shape are already set.
</div>

## Importing

You can import your settings from a table, which must have following format.

| Condition  | color                                               | shape                                    | name                    |
|------------|-----------------------------------------------------|------------------------------------------|-------------------------|
| Condition1 | Leave empty for no color, otherwise a valid R color | -1 for no shape, otherwise a valid R pch | Leave empty for no name |
| Condition2 | ...                                                 | ...                                      | ...                     |
| Condition3 | ...                                                 | ...                                      | ...                     |
| ...        | ...                                                 | ...                                      | ...                     |

<div class="well help-box">
<label>Note</label> Depending on the available settings you can leave out columns. For example the Venn Diagram plots don't support a shape.
This means that <em>no</em> shape setting is available. You can leave out the shape column. If you leave it in, it will be ignored.
</div>
