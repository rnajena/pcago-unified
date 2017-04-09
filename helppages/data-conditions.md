# Conditions

Each cell has a set of conditions that are useful for visualizing data.
For example cells that have been treated with vitamins could have a different color
in the plot than cells without any treatment.

PCAGO allows you to determine the conditions of each cell and set visual parameters for each one.

## Conditions applying to a cell

You have different options to determine which conditions apply to each cell.
The setting for conditions can be found in `Sidebar > Data > Conditions`

### Column names

This is the default option and treats each cell individually.

<div class="well help-box">
<label>Example</label> A cell <code>mono6_n2_ctr</code> has only one condition: <code>mono6_n2_ctr</code>
</div>

### Extract from columns

The column name is separated by a separator (default: `_`). A condition applies to a cell
if it's in this list.

<div class="well help-box">
<label>Example</label> A cell <code>mono6_n2_ctr</code> has following conditions: <code>mono6</code>, <code>n2</code> and <code>ctr</code>
</div>

### Upload

You can also upload your own condition definition. Upload a table with following format:

|       | Condition1    | Condition2 | Condition3 | ... |
|-------|---------------|------------|------------|-----|
| Cell1 | TRUE or FALSE | ...        | ...        | ... |
| Cell2 | ...           | ...        | ...        | ... |
| Cell3 | ...           | ...        | ...        | ... |
| ...   | ...           | ...        | ...        | ... |

Cell1, Cell2, ... are the column names in your read count table.
