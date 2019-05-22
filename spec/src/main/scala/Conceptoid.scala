package io.github.nordicmath.mad.spec

import io.github.nordicmath.mad._
import structure._
import predicate._
import MADType._
import DSL._
import scala.language.postfixOps


object Conceptoid extends Spec {
    
    lazy val Bool : Pointer = mad"$Conceptoids://Booleans"
    lazy val REPR : Pointer = mad"$Conceptoids://Representations"
    
    lazy val T : Pointer = Pointer.Self(mad"$Conceptoids://")
    
    lazy val ConceptoidRef : MADRef = MADRef(mad"$Conceptoids://")
    lazy val CollectionRef : MADRef = ConceptoidRef where (mad"$Conceptoid://collection-structure" exists)
    
    def MachineRef (domain : Pointer*)(codomain : Pointer*) : RichMADType = ConceptoidRef
        .where (mad"$Conceptoid://machine-structure" exists)
        .where (mad"$Conceptoid://machine-structure/value/domain" is domain)
        .where (mad"$Conceptoid://machine-structure/value/codomain" is codomain)
    
    import Information._
    def top = Conceptoids
    def info = Seq(
        // Create booleans and representations collections
        MapNew(mad"$top://", "Booleans"),
        MapNew(mad"$top://", "Representations"),
        
        // Name and description
        Apply(mad"$top://Booleans/name", "Booleans"),
        Apply(mad"$top://Booleans/description", "Boolean value, true or false"),
        Apply(mad"$top://Representations/name", "Representations"),
        Apply(mad"$top://Representations/description", "Ways to represent something"),
        
        // Both have collection-structure, neither have machine-structure
        OptionAssign(mad"$top://Booleans/machine-structure", false),
        OptionAssign(mad"$top://Booleans/collection-structure", true),
        OptionAssign(mad"$top://Representations/machine-structure", false),
        OptionAssign(mad"$top://Representations/collection-structure", true),
        
        // True and False representations
        MapNew(mad"$top://", "True"),
        MapNew(mad"$top://", "False"),
        
        // Name and description
        Apply(mad"$top://True/name", "True"),
        Apply(mad"$top://True/description", "Positive value, true"),
        Apply(mad"$top://False/name", "False"),
        Apply(mad"$top://False/description", "Negative value, false"),
        
        // Both have machine-structure, neither collection-structure
        OptionAssign(mad"$top://True/machine-structure", true),
        OptionAssign(mad"$top://True/collection-structure", false),
        OptionAssign(mad"$top://False/machine-structure", true),
        OptionAssign(mad"$top://False/collection-structure", false),
        
        // Neither is defined everywhere
        Apply(mad"$top://True/machine-structure/value/defined-everywhere", false),
        Apply(mad"$top://False/machine-structure/value/defined-everywhere", false),
        
        // Creating 1 empty slot in domain and codomain
        ListNew(mad"$top://True/machine-structure/value/domain"),
        ListNew(mad"$top://True/machine-structure/value/codomain"),
        ListNew(mad"$top://False/machine-structure/value/domain"),
        ListNew(mad"$top://False/machine-structure/value/codomain"),
        ListStop(mad"$top://True/machine-structure/value/domain"),
        ListStop(mad"$top://True/machine-structure/value/codomain"),
        ListStop(mad"$top://False/machine-structure/value/domain"),
        ListStop(mad"$top://False/machine-structure/value/codomain"),
        
        // Assigning these slots
        ReferenceApply(mad"$top://True/machine-structure/value/domain/0", mad"$top://Booleans"),
        ReferenceApply(mad"$top://True/machine-structure/value/codomain/0", mad"$top://Representations"),
        ReferenceApply(mad"$top://False/machine-structure/value/domain/0", mad"$top://Booleans"),
        ReferenceApply(mad"$top://False/machine-structure/value/codomain/0", mad"$top://Representations"),
        
        // Creating slots for Boolean representations
        MapNew(mad"$top://Booleans/collection-structure/value/representations", "True"),
        MapNew(mad"$top://Booleans/collection-structure/value/representations", "False"),
        MapStop(mad"$top://Booleans/collection-structure/value/representations"),
        
        // There are no representations for REPR
        MapStop(mad"$top://Representations/collection-structure/value/representations"),
        
        // Assigning these slots
        ReferenceApply(mad"$top://Booleans/collection-structure/value/representations/True", mad"$top://True"),
        ReferenceApply(mad"$top://Booleans/collection-structure/value/representations/False", mad"$top://False")
    )
    
    lazy val Conceptoids = MADMap(Conceptoid)
    
    lazy val Conceptoid = MADTree("Conceptoid",
        "name" -> MADString.set(category := "meta"),
        "description" -> MADString.set(category := "meta"),
        "machine-structure" -> MADOption(Machine),
        "collection-structure" -> MADOption(Collection)
    )
    
    lazy val Collection = MADTree("Collection",
        "representations" -> MADMap(MachineRef(T)(REPR))
    )
    
    lazy val Machine = MADTree("Machine",
        "domain" -> MADList(CollectionRef),
        "codomain" -> MADList(CollectionRef),
        "defined-everywhere" -> MADBool
    )
}
