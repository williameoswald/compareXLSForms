# compareXLSForms

Package for comparing two [XLSForm Docs](https://xlsform.org/en/). Currently designed for use with SurveyCTO XLSForm files but should be usable or modifiable for use with ODK forms. 

## Download and install package
Download the library from Github:
```{r}
devtools::install_github("williameoswald/compareXLSForms")
```

Load libraries:
```{r}
library("compareXLSForms")
library("tidyverse")
library("readxl")
library("flextable")
```

## Your inputs
```{r}
form1location <- "filepath to first XLSForm here"
form2location <- "filepath to second XLSForm here"

Note - Please ensure the "label", "hint", "constraint message" columns are named "label:English", "hint:English", and "constraint message:English". Additional language columns (only one additional language at a time for now) can be examined by specifying them as a third argument according to how the language is named in the form (e.g. "Francais" for label:Francais).

# Name output of function "full_compare""
full_compare <- compare_survey(form1location,form2location,language)
```

## compare_survey function tabulates differences between forms for the following checks:
1. Compare content - list items not present in both survey sheets
2. Get row number for each item in form 1 relative to same item's position in form 2
3. Compare type per name
4. Compare calculation fields for calculate items
5. Compare all constraint fields
6. Compare all relevance fields
7. Compare English labels
8. Compare English constraint messages
9. Compare English hints
10. Compare Other language labels
11. Compare Other language constraint messages
12. Compare Other language constraint messages

## Output results from checks
```{r}
tabulate_comparison(1)
tabulate_comparison(2)
tabulate_comparison(3)
tabulate_comparison(4)
tabulate_comparison(5)
tabulate_comparison(6)
tabulate_comparison(7)
tabulate_comparison(8)
tabulate_comparison(9)
tabulate_comparison(10)
tabulate_comparison(11)
tabulate_comparison(12)
```

## To do:
 - Add comparison of choices tab
 - Make cleaner output
 - Functionalise repeated actions 
 
 ## Contributors

Please contact [me](https://www.lshtm.ac.uk/aboutus/people/oswald.william) with any questions or suggestions.

## License

Available for use under a CC BY-NC-SA 4.0 license (https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode).
