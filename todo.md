Management:
* Add package.scala and documentation to everything
* Add tests

Less important:
* Look into using extraction for codecs, or at least use a codec helper for "objects", w/ union codecs
* Implement dependency-relations for information (do we want this?)
* Factoring from core?

Interface:
* Web-interface (!)
* Make questions more natural, not the whole "path"-thing. Use context-object?
* Definitions in Conceptoid.scala: lazy val T = "MADRef to self", lazy val Collection = MADRef where "collection" is defined (perhaps more?), lazy val Bool, lazy val REPR, def Machine(inputs*, outputs*)

Misc:
* Write about category structure on MAD-states (w/ Information as morphisms)
* Look into using same core - different spec - for other kinds of information. Philosophy? Physics? Comp. Sci?

Features:
* Requirements of the types
* MADRef (reference to a conceptoid, w/ predicate)
* MADEnums? Make MADOption an Enum?
* Priority engine
* "User session" object, for meta on qa
* Cartesian product as a collection! How???
* Relations between conceptoids?
* Inference engine! Both "right-hemisphere" and "left-hemisphere"; inference of Information through relations and etc... and inference of questions through analogies 
* Tags! for helping inference engine. Use the weird apply-any method.. (?)
* MADType embeddings

Cleanup:

Spec:
