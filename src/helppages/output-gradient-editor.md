# Gradient editor

Plots like heatmaps require a method to convert a numerical value to a color. 
The included gradient editor allows fully customization of this mapping.

![Screenshot of gradient editor](helppages/gradientEditor.png)

## User interface

The interface contains a list of *gradient stops* that act as reference points for the linear interpolation.
By selecting a stop, you can change its color and the associated *value* that should be mapped to the color.

The values that are set within the gradient stops **must be ordered ascending**. If this is not the case,
the plot will not render.

You can add additional stops by clicking the *Add stop* button. If you have more than two stops, you can remove
the current selected stop by clicking the *Remove this stop* button.

## Importing

Following importers are available:

* **PCAGO gradient CSV.**

### General format of a PCAGO gradient table

| value      | color                                               |
|------------|-----------------------------------------------------|
| value1     | Valid R color                                       |
| value2     | ...                                                 |
| value3     | ...                                                 |
| ...        | ...                                                 |
