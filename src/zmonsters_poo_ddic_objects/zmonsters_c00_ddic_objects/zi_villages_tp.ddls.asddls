@AbapCatalog.sqlViewName: 'ZIVILLAGESTP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Village Business Object'
@Metadata.allowExtensions: true
define view ZI_Villages_TP
  as select from ztm_villages
{
  key village_number              as VillageNumber,
      village_region              as VillageRegion,
      village_address_description as VillageAddressDescription,
      avg_nose_size               as AverageNoseSize,
      avg_asthma_level            as AverageAsthmaLevel,
      @EndUserText.label: 'Last Changed By'
      lastchangedby               as LastChangedBy,
      @EndUserText.label: 'Last Changed On'
      lastchangedon               as LastChangedOn
}
