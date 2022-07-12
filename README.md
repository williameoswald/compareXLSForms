# compareXLSForms

Package for comparing two [XLSForm Docs](https://xlsform.org/en/). Currently designed for use with SurveyCTO XLSForm files but should be usable or modifiable for use with ODK forms. 

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

## To do:
 - Add comparison of choices tab
 - Make cleaner output
 - Functionalise repeated actions 
 
 ## Contributors

Please contact [me](https://www.lshtm.ac.uk/aboutus/people/oswald.william) with any questions or suggestions.

## License

compare_xlsform script is made available for use under a CC BY-NC-SA 4.0 license (https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode).
