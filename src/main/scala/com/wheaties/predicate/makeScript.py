def covariant(x):
  return '-' + x

def lower_bound(left, right):
 return left + ' <: ' + right

def specialized(expr):
  return '@specialized(Int,Long,Float,Double) ' + expr

def arguments(num):
  return ['arg' + str(x) for x in range(1,num+1)]

def _types(num):
  return ['T' + str(x) for x in range(1, num+1)]

def under_types(num):
  return ['T' + x for x in _types(num)]

def pred_types(num):
  those = _types(num)
  under = under_types(num)

  return [lower_bound(x, y) for (x,y) in zip(under, those)]

def typed_args(args, all_types):
  return [x + ': ' + y for (x,y) in zip(args, all_types)]

def named_func(name, func, num):
  those = _types(num)
  that = under_types(num)
  args = arguments(num)
  bound_types = pred_types(num)
  func_types = those[:]
  func_types.append('Boolean')

  typed_argu = ', '.join(typed_args(args, those))
  untyped_argu = ', '.join(args)

  bound_types_str = '[' + ', '.join(bound_types) + ']'
  func_types_str = '[' + ', '.join(func_types) + ']'
  under_types_str = '[' + ', '.join(that) + ']'

  return ''.join(['\tdef ', name, str(bound_types_str), '(that: Function', str(num), func_types_str, ') = new Predicate', str(num), under_types_str, '{\n', \
                  '\t\tdef apply(', typed_argu, ') = ', func(untyped_argu), '\n', \
                  '\t}'])

def func_or(args):
  return 'self(' + args + ') || that(' + args + ')'

def func_and(args):
  return 'self(' + args + ') && that(' + args + ')'

def func_xor(args):
  return 'if(self(' + args + ')) !that(' + args + ') else that(' + args + ')'

def func_nor(args):
  return '!(' + func_or(args) + ')'

def func_nand(args):
  return '!(' + func_and(args) + ')'

def func_nxor(args):
  return 'if(self(' + args + ')) that(' + args + ') else !that(' + args + ')'


def pred_decl(num):
  inner = [('or', func_or), ('and', func_and), ('xor', func_xor), ('nor', func_nor), ('nand', func_nand), ('nxor', func_nxor)]
  funcs = '\n'.join([named_func(x, f, num) for (x, f) in inner])
  those = _types(num)

  pred_types_str = '[' + ', '.join(those) + ']'
  func_types_str = those[:]
  func_types_str.append('Boolean')
  func_types_str = '[' + ', '.join(func_types_str) + ']'

  return ''.join(['trait Predicate' ,str(num), pred_types_str, ' extends Function', func_types_str, '{\n', \
                  '\tself =>\n\n', \
                  funcs, \
                  '\n\toverride toString() = '<predicate', str(num), '>\n', \
                  '\n}\n\n'])

def obj_decl(num):
  those = _types(num)
  pred_types_str = '[' + ', '.join(those) + ']'

  args = arguments(num)
  args_str = ', '.join(args)
  typed_argu = ', '.join(typed_args(args, those))

  return ''.join(['object Predicate', str(num), '{\n', \
                  '\timplicit def not', pred_types_str, ' = new Negation[Predicate', str(num), pred_types_str, ']{\n' \
                  '\t\tdef not(pred: Predicate', str(num), pred_types_str, ') = new Predicate', str(num), pred_types_str, '{\n', \
                  '\t\t\tdef apply(', typed_argu, ') = !pred(', args_str, ')\n',
                  '\t\t}\n', \
                  '\t}\n', \
                  '}\n\n'])

def always_decl(num):
  types = ['Any' for x in range(0,num)]
  args = arguments(num)
  typed_argu = ', '.join(typed_args(args, types))

  return ''.join(['object Always', str(num), ' extends Predicate', str(num), '[', ','.join(types), ']{\n',
                  '\tdef apply(', typed_argu, ') = true\n}\n'])

def never_decl(num):
  types = ['Any' for x in range(0,num)]
  args = arguments(num)
  typed_argu = ', '.join(typed_args(args, types))

  return ''.join(['object Never', str(num), ' extends Predicate', str(num), '[', ','.join(types), ']{\n',
                  '\tdef apply(', typed_argu, ') = false\n}\n'])

def create_all(num):
  return '\n\n'.join([pred_decl(num), obj_decl(num), always_decl(num), never_decl(num)])