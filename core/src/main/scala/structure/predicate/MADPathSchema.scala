package io.github.nordicmath.mad.structure.predicate

import io.github.nordicmath.mad._
import structure._
import MADType._

case class MADPathSchema(prepath : MADPath) {
    val length = prepath.instructions.length + 1
    val madtype = prepath.on
    val pointertype = prepath.madtype.inner match {
        case MADList(param) => param
        case MADMap(param) => param
        case _ => throw MADException.MADTypeNotIterable(prepath.madtype)
    }
    
    def check(subpath : MADPath) : Boolean = {
        subpath.on == madtype &&
        subpath.madtype == pointertype &&
        subpath.instructions.startsWith(prepath.instructions) &&
        subpath.instructions.length == length
    }
    
    def fit(path : MADPath) : MADPath = if (path.on == prepath.on && path.instructions.length >= length) path.take(length)
        else throw MADException.MADPathSchemaFitException(path, prepath)
}

