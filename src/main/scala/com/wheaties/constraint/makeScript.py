def create(size):
  args = ",".join(['x' + str(x) for x in range(0,size)])
  generics = [x for x in "ABCDEFGHIJKLMNOPQRSTUVWXYZ"][:size]
  nextOne = [x for x in "ABCDEFGHIJKLMNOPQRSTUVWXYZ"][size]
  gens = ",".join(generics)
  func = "Function" + str(size) + "[" + gens + ",Boolean]"
  fillin = [
    "trait Constraint" + str(size) + "[" + gens + "]extends Predicate1[" + func + "]{\n",
    "\tdef constrain[" + nextOne + "](that: Constraint1[" + nextOne + "]) = new Constraint" + str(size+1) + "[" + gens + "]{\n",
    "\t\tdef apply(pred: " + func + ") = Constraint" + str(size) + ".this((" + args + ") => that(pred(" + args + ",_)))\n",
    "\t}\n\n",
    "\tdef suchThat(pred: " + func + ") = apply(pred)\n",
    "}"]
  return ''.join(fillin)

def make(size):
  FILE = open("Constraint" + str(size) + ".scala", "w")
  FILE.write(create(size))
  FILE.close()

def implicits(size):
  generics = ','.join([x for x in "ABCDEFGHIJKLMNOPQRSTUVWXYZ"][:size])
  pred = ''.join(["Predicate1", str(size), "[", generics, "]"])
  func = ''.join(["Function", str(size), "[", generics, ",Boolean]"])
  predConv = [
    "implicit def pred2const", str(size), "(pred: Predicate1[", func, "]) = new Constraint", str(size), "[", generics, "]{\n",
    "\tdef apply(arg: ", func, ") = pred(arg)\n",
    "}\n"]
  funcConv = [
    "implicit def func2const", str(size), "(func: ", func, " => Boolean) = new Constraint", str(size), "[", generics, "]{\n",
    "\tdef apply(arg: ", func, ") = func(arg)\n",
    "}\n"]

  return ''.join(predConv) + ''.join(funcConv)
