# Common file formats

This page gives you a brief overview about common file formats.

## CSV
Character Separated Value (CSV) files represent tables that are read from plain text files.
A line in the text file represents a row, while the columns are determined by
splitting each line at a separator.
If a separating character is valid data (like `1,000` in case of *comma* separated files), the
value is put into quotes.

**Example**

Following table

| ID | A   | B   | C   |
|----|-----|-----|-----|
| G1 | 4,54 | 242 | 121 |
| G2 | 122 |     | 454 |

can be written as following in CSV format if the separator is a comma:

```
ID,A,B,C
G1,"4,54",242,121
G2,122,,454
```

If the separator is a whitespace, it can be written as following:

```
ID A B C
G1 454 242 121
G2 122 "" 454
```
