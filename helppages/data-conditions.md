Each cell has a set of conditions that are useful for visualizing data.
For example cells that have been treated with vitamins could have a different color
in the plot than cells without any treatment.

PCAGO allows you to determine the conditions of each cell and set visual parameters for each one.

## Conditions applying to a cell

You have different options to determine which conditions apply to each cell.
The setting for conditions can be found in `Sidebar > Data > Conditions`

### Column names

This is the default option and treats each cell individually.

**Example**
A cell `mono6_n2_ctr` has only one condition: `mono6_n2_ctr`

### Extract from columns

The column name is separated by a separator (default: `_`). A condition applies to a cell
if it's in this list.

**Example**
A cell `mono6_n2_ctr` has following conditions: `mono6`, `n2` and `ctr`

### Upload

You can also upload your own condition definition. Upload a table with following format:

|       | Condition1    | Condition2 | Condition3 | ... |
|-------|---------------|------------|------------|-----|
| Cell1 | TRUE or FALSE | ...        | ...        | ... |
| Cell2 | ...           | ...        | ...        | ... |
| Cell3 | ...           | ...        | ...        | ... |
| ...   | ...           | ...        | ...        | ... |

Cell1, Cell2, ... are the column names in your read count table.
