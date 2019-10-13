@AbapCatalog.sqlViewName: 'ZV_VILLAGES'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Village Master Data'
define view ZCDS_VILLAGES
  as select from zmn_villages as Villages
{
  key Villages.village_number   as VillageNumber,
      Villages.village_region   as VillageRegion,
      Villages.village_address  as VillageAddress,
      Villages.avg_nose_size    as AverageNoseSize,
      Villages.avg_asthma_level as AverageAsthmaLevel
}
