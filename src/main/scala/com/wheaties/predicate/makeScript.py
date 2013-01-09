def covariant(x):
  return '-' + x

def lower_bound(left, right):
 return left + ' >: ' + right

def specialized(expr):
  return '@specialized(Int,Long,Float,Double) ' + expr

def arguments(exprs):
  return ['arg' + str(x) for x in range(0,len(exprs))]

def apply_decl(exprs):
  def func_arg(tup):
    return tup[1] + ': ' + tup[0]

  return 'def apply(' + ', '.join(map(func_arg, zip(exprs,arguments(exprs)))) + ')'

def compound_decl(lexprs, rexprs, func_rel):
  length = len(lexprs)

  return 'new CompoundPredicate' + str(length) + '(p, q){\n\t' + apply_decl(lexprs) + ' = ' + func_rel(', '.join(arguments(lexprs))) + '\n}'

def func_or(args):
  return 'p(' + args + ') || q(' + args + ')'

def func_and(args):
  return 'p(' + args + ') && q(' + args + ')'

def func_xor(args):
  return 'if(p(' + args + ')) !q(' + args + ') else q(' + args + ')'

def func_nor(args):
  return '!(' + func_or(args) + ')'

def func_nand(args):
  return '!(' + func_and(args) + ')'

def func_nxor(args):
  return 'if(p(' + args + ')) q(' + args + ') else !q(' + args + ')'

def connective_func(lexprs, rexprs, op):
  length = len(lexprs)
  return ''.join(['def ', op, '(p: Predicate', str(length), '[', ','.join(lexprs), '], q: Predicate', str(length), '[',
                  ','.join(rexprs), '])'])

def connective_decl(num):
   lexprs = ['T' + str(x) for x in range(0,num)]
   rexprs = ['Q' + str(x) for x in range(0,num)]
   bounds = [lower_bound(y,x) for (x,y) in zip(lexprs,rexprs)]
   ltypes = ','.join(lexprs)
   rtypes = ','.join(rexprs)
   pairs = [('or', func_or), ('and', func_and), ('xor', func_xor), ('nand', func_nand), ('nxor', func_nxor), ('nor', func_nor)]
   def inner_func(arg):
     (name, func) = arg
     return connective_func(lexprs, rexprs, name) + ' = ' + compound_decl(lexprs, rexprs, func) + '\n'

   return ''.join(['protected[predicate] implicit def conn[', ','.join(bounds), '] = new Connection[Predicate', str(num),
                  '[', ltypes, '],Predicate', str(num), '[', rtypes, '],Predicate', str(num), '[', ltypes, ']]{\n',
                  ''.join(map(inner_func, pairs)), '}'])

def negation_decl(num):
  exprs = ['T' + str(x) for x in range(0,num)]
  types = ','.join(exprs)
  return ''.join(['protected[predicate] implicit def not = new Negation[Predicate', str(num), '[', types, ']]{\n',
                 'def not(pred: Predicate', str(num), '[', types, ']) = new Predicate', str(num), '[', types, ']{\n',
                 apply_decl(exprs), '= !pred(', ','.join(arguments(exprs)), ')\n}\n}'])

def pred_decl(num):
  exprs = ['T' + str(x) for x in range(0,num)]
  return 'trait Predicate' + str(num) + '[' + ', \n'.join(map(lambda x: specialized(covariant(x)), exprs)) + \
    '] extends Function' + str(num) + '[' + ','.join(exprs) + ',Boolean] with PredicateLike[Predicate' + \
    str(num) + '[' + ','.join(exprs) + ']]{\n'

def make_predicate(num):
  return ''.join([pred_decl(num), connective_decl(num), negation_decl(num), '\n}'])

def cpred_decl(num):
  lexprs = ['T' + str(x) for x in range(0,num)]
  rexprs = ['Q' + str(x) for x in range(0,num)]
  bounds = [lower_bound(y,x) for (x,y) in zip(lexprs,rexprs)]
  return ''.join(['abstract class CompoundPredicate', str(num), '[', ',\n'.join(map(specialized,lexprs)),
                  ',\n'.join(map(specialized,bounds)), '](p: Predicate', str(num), '[', ','.join(lexprs),
                  '], q: Predicate', str(num), '[', ','.join(rexprs), ']) extends Predicate', str(num), '[',
                  ','.join(lexprs), ']'])

def always_decl(num):
  types = ['Any' for x in range(0,num)]
  return ''.join(['object Always', str(num), ' extends Predicate', str(num), '[', ','.join(types), ']{\n',
                  apply_decl(types), ' = true\n}'])

def never_decl(num):
  types = ['Any' for x in range(0,num)]
  return ''.join(['object Never', str(num), ' extends Predicate', str(num), '[', ','.join(types), ']{\n',
                  apply_decl(types), ' = false\n}'])

def create_all(num):
  return '\n\n'.join([make_predicate(num), cpred_decl(num), always_decl(num), never_decl(num)])