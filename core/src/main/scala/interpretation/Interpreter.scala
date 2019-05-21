package io.github.nordicmath.mad.interpretation

import io.github.nordicmath.mad._
import structure._
import Information._
import MADException._


trait Interpreter {
    def interpret (str : String) : Seq[Information]
}

object Interpreter {
    
    def parseBool(str : String) : Boolean = str match {
        case "true" | "yes" => true
        case "false" | "no" => false
        case _ => throw BooleanInput
    }
    
    def parseInt(str : String) : Int = try str.toInt catch {
        case _ : IllegalArgumentException => throw IntegerInput
    }
    
    def parseMADPath(str : String, madtype : RichMADType) : MADPath = {
        val prefix = "mad://"
        
        if(!str.startsWith(prefix)) throw MADException.MADPathInput
        val instr = str.drop(prefix.length).split("/")
        val instructions : Seq[String] = instr.filter(_.length > 0)
        
        MADPath(madtype, instructions)
    }
    
    
    def stringInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = Seq(Apply[String](path, str))
    }
    
    def intInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = Seq(Apply[Int](path, parseInt(str)))
    }
    
    def boolInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = Seq(Apply[Boolean](path, parseBool(str)))
    }
    
    def optionInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = Seq(OptionAssign(path, parseBool(str)))
    }
    
    def listInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = if (parseBool(str)) Seq(ListNew(path)) else Seq()
    }
    
    def mapInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = Seq(MapNew(path, str))
    }
    
    def enumInterpreter(path : MADPath) : Interpreter = new Interpreter {
        def interpret (str : String) = Seq(EnumAssign(path, parseInt(str)))
    }
    
    def pathInterpreter(path : MADPath) = new Interpreter {
        val MADType.MADRef(schema, predicate) = path.madtype.inner.asInstanceOf[MADType.MADRef]
        def interpret (str : String) = {
            val value = parseMADPath(str, schema.madtype)
            
            Seq(ReferenceApply(path, value))
        }
    }
}
