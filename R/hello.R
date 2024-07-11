# Normal
fact1 <- function(N){
  f <- 1
  i <- 1
  while(i <= N){
    f <- f*i
    i <- i+1
  }
  return(f)
}



# Recursividad
f_recursiva <- function(N){
  if(N == 0){
    return(1)
  } else {
    return(N*f_recursiva(N-1))
  }
}

# Mostrar los factoriales

cat("Ingrese un numero:\n")
N <- readLines(n=1)
N <- as.numeric(N)

f2 <- fact1(N)
f3 <- f_recursiva(N)
cat("El factorial de: ",N,"es: ",f2,"\n")
cat("Con recursividad es: ",f3)

# Multiplicacion de 2 numeros por sumas sucesivas
sum_suce <- function(b,a){
  suma1 <- 0
  i <- 1
  while (i<=b) {
    suma1 <- suma1 + a
    i <- i+  1
  }
  return(suma1)
}

sum_suce1 <- function(b,a){
  suma1 <- 0
  i <- 1
  while (i<=a) {
    suma1 <- suma1 + b
    i <- i+  1
  }
  return(suma1)
}

recursiva_suma <- function(a,b){
  if(a==0){
    return(0)
  } else {
    return(b+recursiva_suma(a-1,b))
  }
}

recursiva_suma <- function(a,b){
  if(b==0){
    return(0)
  } else {
    return(a+recursiva_suma(b-1,a))
  }
}

cat("Ingrese un numero:\n")
a <- readLines(n=1)
a <- as.numeric(a)

cat("Ingrese un numero:\n")
b <- readLines(n=1)
b <- as.numeric(b)

c <- sum_suce(b,a)
c1 <- sum_suce1(b,a)
c2 <- recursiva_suma(a,b)
c3 <- recursiva_suma(a,b)
cat("La multiplicacion M1 es: ",c,"\n")
cat("La multiplicacion M2 es: ",c1,"\n")
cat("La multiplicacion M3 es: ",c2,"\n")
cat("La multiplicacion M4 es: ",c3)

# Función para sumar a mediante sumas sucesivas
sum_sec_rec <- function(b, a) {
  if (b == 0) {
    return(0)
  } else {
    return(a + sum_sec_rec(b - 1, a))
  }
}

# Función para calcular potencia mediante multiplicaciones sucesivas
potencia_suc_rec <- function(base, exponente) {
  if (exponente == 0) {
    return(1)
  } else {
    return(sum_sec_rec(base, potencia_suc_rec(base, exponente - 1)))
  }
}

# Pedir al usuario que ingrese dos números
cat("Ingrese dos números separados por espacio (base exponente):\n")
entrada <- scan(what = numeric(), nmax = 2)

if (length(entrada) != 2) {
  cat("Debe ingresar exactamente dos números.\n")
} else {
  base <- entrada[1]
  exponente <- entrada[2]

  # Calcular la potencia usando multiplicaciones sucesivas de sumas
  resultado <- potencia_suc_rec(base, exponente)

  # Mostrar el resultado
  cat("El resultado de", base, "elevado a", exponente, "mediante multiplicaciones sucesivas de sumas es:", resultado, "\n")
}

# Función para sumar un número x, n veces
suma_n_veces <- function(x, n) {
  if (n == 0) {
    return(0)
  } else {
    return(x + suma_n_veces(x, n - 1))
  }
}

# Función para calcular el factorial usando sumas sucesivas
factorial_sumas <- function(N) {
  if (N == 0) {
    return(1)
  } else {
    return(suma_n_veces(N, factorial_sumas(N - 1)))
  }
}

# Pedir al usuario que ingrese un número
cat("Ingrese un número para calcular su factorial mediante sumas:\n")
N <- as.numeric(readLines(n = 1))

# Calcular factorial mediante sumas recursivas
resultado <- factorial_sumas(N)

# Mostrar el resultado
cat("El factorial mediante sumas recursivas es:", resultado, "\n")
