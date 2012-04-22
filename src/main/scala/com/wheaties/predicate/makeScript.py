letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

def parentType(x):
  return ', '.join(y for y in letters[:x])

def covariant(x):
  return ', '.join("-" + y for y in letters[:x])

def funcType(x):
  return ', '.join(2*y + " <: " + y for y in letters[:x])

def otherType(x):
  return ', '.join(2*y for y in letters[:x])

logics = ["or", "orNot", "and", "andNot", "xor", "nxor", "nand", "nor"]
def func(name, x):
  return ''.join(["\tdef ", name, "[", funcType(x), "](", "that: Predicate", str(x), "[", otherType(x), "]) = ", name.capitalize(), str(x), "(this, that)\n"])

def applyFunc(x):
  args = ', '.join(["arg" + str(e) + ": " + y for (e,y) in enumerate(letters[:x])])
  return ''.join(["\tdef apply(", args, "):Boolean\n"])

def trait(x):
  memberFunc = ''.join([func(name,x) for name in logics])
  definition = ["trait Predicate", str(x), "[", covariant(x), "] extends Function", str(x), "[", parentType(x), ", Boolean] {\n", memberFunc, applyFunc(x), "}"]
  return ''.join(definition)

def compound(x):
  definition = ["trait CompoundPredicate", str(x), "[", covariant(x), "] extends Predicate", str(x), "[", funcType(x), "] {\n", "\tval pred1: Predicate", str(x), "[", funcType(x), "]\n\tval pred2: Predicate", str(x), "[", funcType(x), "]\n}"]
  return ''.join(definition)
