# Common file formats

This page gives you a brief overview about common file formats.

## CSV
Comma Separated Value (CSV) files represent tables that are read from plain text files.
A line in the text file represents a row, while the columns are determined by
splitting each line at a separator (in this case a comma).
If a comma (or any separating character) is valid data (like `1,000`), the
value is put into quotes.

**Example**

Following table

| ID | A   | B   | C   |
|----|-----|-----|-----|
| G1 | 4,54 | 242 | 121 |
| G2 | 122 |     | 454 |

in CSV can be written as following:

```
ID,A,B,C
G1,"4,54",242,121
G2,122,,454
```

## TSV
Tab Separated Value (TSV) is like CSV, but separated with a tabulator space or
a normal whitespace.

**Example**

Following table

| ID | A   | B   | C   |
|----|-----|-----|-----|
| G1 | 454 | 242 | 121 |
| G2 | 122 |     | 454 |

in TSV can be written as following:

```
ID A B C
G1 454 242 121
G2 122 "" 454
```
