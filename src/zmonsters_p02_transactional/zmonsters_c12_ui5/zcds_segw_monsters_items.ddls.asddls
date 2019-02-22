@AbapCatalog.sqlViewName: 'ZV_MN_ITEMS_SEGW'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Monster Items CDS View for SEGW'
@ObjectModel.modelCategory: #BUSINESS_OBJECT 
@ObjectModel.writeEnabled: true
@ObjectModel.writeActivePersistence: 'ZTMONSTER_ITEMS'
@ObjectModel.createEnabled: true
@ObjectModel.deleteEnabled: true
@ObjectModel.updateEnabled: true
define view Zcds_Segw_Monsters_Items as select from ztmonster_items as monster_items
association [1..1] to Zcds_Segw_Monsters_Header as _monster_header
on monster_items.db_key =_monster_header.db_key {
@ObjectModel.association.type: [#TO_COMPOSITION_ROOT,
                                #TO_COMPOSITION_PARENT]
_monster_header,
key monster_items.db_key,
monster_items.monster_number,
monster_items.monster_item,
monster_items.part_category,
monster_items.part_quantity
}
