


# 🌿 Lógica y Programación (UGR · Curso 2025-2026)

> Ejercicios teórico-prácticos en **Lambda Cálculo**, **Lógica Combinatoria** y **Haskell** correspondientes a la asignatura **Lógica y Programación** — Grado en Ingeniería Informática, Universidad de Granada.

<p align="center">
  <a href="https://github.com/a-egealopez">
    <img src="https://github.com/a-egealopez.png" width="90px" style="border-radius:50%;" alt="a-egealopez"/>
  </a>
  &nbsp;&nbsp;&nbsp;&nbsp;
  <a href="https://github.com/Nicram01">
    <img src="https://github.com/Nicram01.png" width="90px" style="border-radius:50%;" alt="nicolas"/>
  </a>
</p>

<p align="center">
  <b>Desarrolladores:</b>  
  <a href="https://github.com/a-egealopez">a-egealopez</a> ·
  <a href="https://github.com/Nicram01">Nicram01</a>
</p>

## 🧩 Contenido del repositorio

### 📘 Ejercicios Teóricos

📂 **Carpeta:** [`/lamda-calculus & CL`](https://github.com/a-egealopez/haskel101/tree/main/lambda-calculus%20%26%20CL)

| Nº | Archivo | Descripción |
|:--:|----------|-------------|
| 1 | `t1_notacion_de_bruijn.pdf` | Exposición y desarrollo del tema de la *Notación de De Bruijn*. |
| 2 | `t2_demostracion_KS.pdf` | Demostración de que para todo λ-término N, λx:xKN ≡ λx:xSN, con K ≡ λxy.x y S ≡ λxyz.xz(yz). |
| 3 | `t3_grafo_WWW.pdf` | Construcción y razonamiento del grafo \( G_λ(WWW) \), donde W ≡ λxy.xyy. |
| 4 | `t4_busqueda_M.pdf` | Hallar un λ-término M tal que \( G_λ(M) \) sea un grafo concreto dado. |
| 5 | `t5_combinador_punto_fijo.pdf` | Demostraciones relativas a operadores de punto fijo usando los términos:<br>G ≡ λyx.x(yx) y M ≡ (λxy.y(xxy))(λxy.y(xxy)). |
| 6 | `t6_combinador_Y.pdf` | Demostración de que GY = Y, siendo \( Y ≡ λy.(λx.y(xx))(λx.y(xx)) \). |
| 7 | `t7_sucesion_Yn.pdf` | Demostración de que la sucesión \( {Y^n} \) definida recursivamente por \( Y^n = Y^{n-1}G \) (con \( Y^0 = Y \)) es una familia de combinadores de punto fijo. |
| 8 | `t8_conversion_CL.pdf` | Obtención razonada del término equivalente en *Lógica Combinatoria* de (λxy.xyy), es decir, \((λxy.xyy)_{CL}\). |
| 9 | `t9_relacion_lambda_CL.pdf` | Esquematización de la relación entre el sistema λ y la Lógica Combinatoria. |

### 💠 Haskell

📂 **Carpeta:** [`/haskell & prolog`](https://github.com/a-egealopez/haskel101/tree/main/haskell%20%26%20prolog)

Implementaciones funcionales de los problemas del bloque 2 del documento de tareas:

| Archivo | Descripción |
|----------|-------------|
| `heron.hs` | Cálculo del área de un triángulo con la fórmula de Herón y método de Newton-Raphson. |
| `vigenere.hs` | Criptosistema de Vigenère y pequeño laboratorio de análisis criptográfico. |

---

## ⚙️ Requisitos

Para ejecutar los proyectos necesitarás:

- 🟣 [**GHC**](https://www.haskell.org/ghc/) — compilador de Haskell
- 🟦 [**Visual Studio Code**](https://code.visualstudio.com/) con las extensiones:
  - *Haskell* (Haskell Foundation)

---

## ▶️ Ejecución rápida

```bash
####################################################
#################### En Haskell ####################
####################################################

# 1️⃣ Abrir GHCi en la terminal
ghci

# 2️⃣ Cargar un archivo Haskell
:l fichero.hs

# 3️⃣ Ejecutar funciones definidas en el archivo
funcion 4

# 4️⃣ Para recargar el archivo tras hacer cambios
:r

# 5️⃣ Para salir de GHCi
:q
```