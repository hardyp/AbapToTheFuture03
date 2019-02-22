@AbapCatalog.sqlViewName: 'ZV_MONSTERS'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Monster CDS View'
// Listing 07.28 : Complete DDL Source for Generating CDS View
define view Zcds_Monsters_Join
  as select from    ztmonster_header as monster_header
    inner join      ztmonster_items  as monster_items on monster_header.monster_number = monster_items.monster_number
    left outer join ztmonster_pets   as monster_pets  on monster_header.monster_number = monster_pets.owner
{
  key monster_header.monster_number          as monster_number,
      monster_header.name                    as monster_name,
      SUBSTRING( monster_header.name, 1, 1 ) as first_initial,
      case monster_header.evilness
      when 'BANK' then 'REALLY SCARY'
      when 'VERY' then
        case monster_header.strength
          when 100 then 'SCARY'
          else 'NOT SO SCARY REALLY'
        end
      when 'EVIL' then 'SLIGHTLY SCARY'
      else 'NOT REALLY SCARY AT ALL'
      end                                    as scariness_description
}
where
      monster_header.sanity_percentage < 10
  and monster_header.color             = 'GREN'
