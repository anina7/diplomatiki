def parse(expr):
	#get expression and turn it into nested loops on every "(" + ")"
	#CODE FROM: https://www.reddit.com/r/learnprogramming/comments/20lggh/python_homework_splitting_a_string_by_nested/
    def _helper(iter):
        items = []
        for item in iter:
            if item == '(':
                result, closeparen = _helper(iter)
                if not closeparen:
                    raise ValueError("Error: Unbalanced parentheses")
                items.append(result)
            elif item == ')':
                return items, True
            else:
                items.append(item)
        return items, False
    return _helper(iter(expr))[0]
	



from collections import OrderedDict 

def getInfo(data):
	#get original data and remove special characters
	minus1 = data.replace("*", "")
	minus2 = minus1.replace("(", "")
	minus3 = minus2.replace(")", "")
	minus4 = minus3.replace("[", "")
	minus5 = minus4.replace("]", "")
	return "".join(set(minus5))
	