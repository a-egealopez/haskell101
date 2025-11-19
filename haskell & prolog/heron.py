###################################################################
#################### Discernir si es triángulo ####################
###################################################################

def es_triangulo(t):
    a, b, c = t
    return a+b>c and a+c>b and b+c>a # verificar desigualdad triangular


###################################################################
#################### Método de Newton-Raphson #####################
###################################################################

def f(x, y):
    return x**2 - y

def df(x):
    return 2*x

def metodo_newton(y, it=100):
    """
    Método de Newton para encontrar la raíz cuadrada de r
    
    Args:
        yr: número del cual encontrar la raíz cuadrada
        x0: valor inicial
        it: número máximo de iteraciones
    
    Returns:
        float: aproximación de la raíz cuadrada de r
    """
    x = max(1, y/2) # recomendación de la literatura    
    for i in range(it):
        x = x - f(x,y) / df(x)    
    return x

###################################################################
######################## Fórmula de Herón ########################
###################################################################

def heron(t):
    if not es_triangulo(t):
        return -1
    a, b, c = t 
    s = (a+b+c)/2
    return metodo_newton(s*(s-a)*(s-b)*(s-c))

###################################################################
############################## Main ###############################
###################################################################

def main():
    #r = 7
    #z = metodo_newton(r)
    #print(f"La raíz de {r} es {z}.")

    #t = (3,4,5)
    #b = es_triangulo(t)
    #print(f"La terna {t} es triángulo: {b}.")

    h = (3,4,5)
    a = heron(h)
    print(f"El área del triángulo {h} es {a}.")

if __name__ == "__main__":
    main()