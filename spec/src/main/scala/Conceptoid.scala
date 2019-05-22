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
        MapNew(mad"$top://", "Booleans"),
        MapNew(mad"$top://", "Representations"),
        Apply(mad"$top://Booleans/name", "Booleans"),
        Apply(mad"$top://Booleans/description", "Boolean value, true or false"),
        Apply(mad"$top://Representations/name", "Representations"),
        Apply(mad"$top://Representations/description", "Ways to represent something"),
        OptionAssign(mad"$top://Booleans/machine-structure", false),
        OptionAssign(mad"$top://Booleans/collection-structure", true),
        OptionAssign(mad"$top://Representations/machine-structure", false),
        OptionAssign(mad"$top://Representations/collection-structure", true),
        MapNew(mad"$top://", "True"),
        MapNew(mad"$top://", "False"),
        Apply(mad"$top://True/name", "True"),
        Apply(mad"$top://True/description", "Positive value, true"),
        Apply(mad"$top://False/name", "False"),
        Apply(mad"$top://False/description", "Negative value, false"),
        OptionAssign(mad"$top://True/machine-structure", true),
        OptionAssign(mad"$top://True/collection-structure", false),
        OptionAssign(mad"$top://False/machine-structure", true),
        OptionAssign(mad"$top://False/collection-structure", false),
        Apply(mad"$top://True/machine-structure/value/defined-everywhere", false),
        Apply(mad"$top://False/machine-structure/value/defined-everywhere", false),
        ListNew(mad"$top://True/machine-structure/value/domain"),
        ListNew(mad"$top://True/machine-structure/value/codomain"),
        ListNew(mad"$top://False/machine-structure/value/domain"),
        ListNew(mad"$top://False/machine-structure/value/codomain"),
        ReferenceApply(mad"$top://True/machine-structure/value/domain/0", mad"$top://Booleans"),
        ReferenceApply(mad"$top://True/machine-structure/value/codomain/0", mad"$top://Representations"),
        ReferenceApply(mad"$top://False/machine-structure/value/domain/0", mad"$top://Booleans"),
        ReferenceApply(mad"$top://False/machine-structure/value/codomain/0", mad"$top://Representations"),
        MapNew(mad"$top://Booleans/collection-structure/value/representations", "True"),
        MapNew(mad"$top://Booleans/collection-structure/value/representations", "False"),
        ReferenceApply(mad"$top://Booleans/collection-structure/value/representations/True", mad"$top://True"),
        ReferenceApply(mad"$top://Booleans/collection-structure/value/representations/False", mad"$top://False")
    )
    
    lazy val Conceptoids = MADMap(Conceptoid)
    
    lazy val Conceptoid = MADTree("Conceptoid",
        "name" -> MADString,
        "description" -> MADString,
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
    
    lazy val Representation = MADTree("Representation",
        "description" -> MADString,
        "schema" -> MADString,
        "faithful" -> MADBool,
        "surjective" -> MADBool
    )
}
