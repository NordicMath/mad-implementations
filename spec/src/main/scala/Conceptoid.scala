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
    def info = Seq[Seq[Information]](
        
        newConceptoid("Booleans", "Boolean value, true or false", machineStructure = Some(false), collectionStructure = Some(true)),
        newConceptoid("Representations", "Ways to represent something", machineStructure = Some(false), collectionStructure = Some(true)),
        
        newConceptoid("True", "Positive value, true", machineStructure = Some(true), collectionStructure = Some(false)),
        newConceptoid("False", "Negative value, false", machineStructure = Some(true), collectionStructure = Some(false)),
        
        // Neither is defined everywhere
        Seq(
            Apply(mad"$top://True/machine-structure/value/defined-everywhere", false),
            Apply(mad"$top://False/machine-structure/value/defined-everywhere", false)
        ),
        
        // Creating 1 empty slot in domain and codomain
        {
            for {
                name <- Seq("True", "False")
                (property, value) <- Seq(("domain", "Booleans"), ("codomain", "Representations"))
            } yield createListSlots(mad"$top://$name/machine-structure/value/$property", 1) :+
                ReferenceApply(mad"$top://$name/machine-structure/value/$property/0", mad"$top://$value")
        }.flatten,
        
        Seq(
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
    ).flatten
    
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
    
    import Information._
    def newConceptoid(name : String, description : String, machineStructure : Option[Boolean] = None, collectionStructure : Option[Boolean] = None) : Seq[Information] = Seq(
        MapNew(mad"$top://", name),
        Apply(mad"$top://$name/name", name),
        Apply(mad"$top://$name/description", description)) ++
        machineStructure.map(OptionAssign(mad"$top://$name/machine-structure", _)).toSeq ++
        collectionStructure.map(OptionAssign(mad"$top://$name/collection-structure", _)).toSeq
    
    def createListSlots(path : MADPath, amt : Int) = Seq.fill(amt)(ListNew(path)) :+ ListStop(path)
    
}
