package fpinscala

import org.scalatest.{EitherValues, FlatSpec, Matchers, OptionValues}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

abstract class AbstractSpec
	extends FlatSpec
	with Matchers
	with OptionValues
	with EitherValues
  with GeneratorDrivenPropertyChecks
