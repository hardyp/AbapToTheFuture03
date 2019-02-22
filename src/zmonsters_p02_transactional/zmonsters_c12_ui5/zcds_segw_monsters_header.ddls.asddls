@AbapCatalog.sqlViewName: 'ZV_MONSTER_SEGW'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Monster Header CDS View for SEGW'
@ObjectModel.compositionRoot: true
@ObjectModel.modelCategory: #BUSINESS_OBJECT
@ObjectModel.writeEnabled: true
@ObjectModel.writeActivePersistence: 'ZTMONSTER_HEADER'
@ObjectModel.createEnabled: true
@ObjectModel.deleteEnabled: true
@ObjectModel.updateEnabled: true
@ObjectModel.transactionalProcessingEnabled: true
@OData.publish: true
@UI.headerInfo.typeName: 'Monster'
@UI.headerInfo.typeNamePlural: 'Monsters'
@UI.headerInfo.title: { value: 'monster_number' }
@UI.headerInfo.description: { value: 'name' }
define view Zcds_Segw_Monsters_Header as select from ztmonster_header as monster_header
association [1..*] to Zcds_Segw_Monsters_Items as _monster_items
on monster_header.db_key =_monster_items.db_key {
key monster_header.db_key,
@ObjectModel.association.type: #TO_COMPOSITION_CHILD
_monster_items,
monster_header.monster_number,
@UI.lineItem.position: 10
@UI.lineItem.importance: #HIGH
@UI.lineItem.dataAction: 'HowlatMoon'
monster_header.name,
@UI.lineItem.position: 20
@UI.lineItem.importance: #HIGH
monster_header.sanity_percentage,
monster_header.color,
@UI.selectionField.position: 10
monster_header.strength,
monster_header.hat_size,
monster_header.scariness,
monster_header.no_of_heads,
monster_header.evilness
}
