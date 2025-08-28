@EndUserText.label: 'Cats'
define view entity ZC_CAT
  as select from zcat_table
{
  key id,
      name,
      kind,
      energy,
      toys_count
}
