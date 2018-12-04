package io.github.nordicmath.mad.conceptoids

case class RichMADType(inner : MADType, priority : Int = 5){
    def priority(p : Int) = copy(priority = p)
}
