# Simulacro de Parcial

## Enunciado

https://docs.google.com/document/d/e/2PACX-1vTON2Wg8ry_rs4t3tqQXjcdN_6TPbv7DvHFWRKFuuOpyD89PKUdFnOkeAESymJuAYWZ2KhsqH2dZnKq/pub

## Estudiante

**Nombre y apellido:** -completar acá-

**Número de legajo:** -completar acá-

## Tiempo del parcial

Entre 14:15 y 17:30.

## Reglas del parcial

https://docs.google.com/document/d/1dbA_-AVJcgffnTDwv7ysnCrKvyep7nzTsftLd4wPEZ8/edit

## Que hacer cuando terminan el parcial

- Crear un issue etiquetando a sus tutores así les llega una notificación y se corrigen y les dejan feedback ahí.
![](https://i.imgur.com/ypeXpBw.gif)

## Ayuda

Si tienen dudas con Haskell pueden ayudarte todo el tiempo con esta documentación

- [Guía de lenguajes](https://docs.google.com/document/d/1oJ-tyQJoBtJh0kFcsV9wSUpgpopjGtoyhJdPUdjFIJQ/edit?usp=sharing), un resumen de las principales funciones que vienen con Haskell.
- [Hoogle](https://www.haskell.org/hoogle/), un motor de búsqueda específico para Haskell.

Aparte, siempre pueden preguntar a sus ayudantes en discord!

Y para comenzar a trabajar con Git les recomendamos [este apunte inicial de Git](https://docs.google.com/document/d/1ozqfYCwt-37stynmgAd5wJlNOFKWYQeIZoeqXpAEs0I/edit) o estos videos donde se explica como usar Git:
- [Parte 1: Qué es GIT y cómo clonar el repo basado en GitHub classroom](https://www.youtube.com/watch?v=rRKe7l-ZNvM)
- [Parte 2: Uso básico de GIT con status, add, reset, commit, push](https://www.youtube.com/watch?v=OgasfM5qJJE)
- [Parte 3: Resolución de conflictos](https://www.youtube.com/watch?v=sKcN7cWFniw)

### Probando cosas por consola

La forma que recomendamos de resolver la kata es no programar todo de una y después ver si anda, si no ir probando en cada paso a medida que van programando cada función.

Para esto, les recomiendo que usen mucho `stack ghci` para probar cosas por consola, y vuelvo a linkear [esta página](https://github.com/pdep-utn/enunciados-miercoles-noche/blob/master/pages/haskell/trabajo.md#comandos-%C3%BAtiles) donde se explican un par de cositas de como usar `ghci`.

### Testeo automatizado

Nuestra solución tiene que estar escrita en el archivo `Library.hs` del directorio `src`, entonces podemos correr pruebas **automatizadas** en la terminal:

```bash
stack test
```

También pueden usar ghcid para correr los tests automáticamente como se explica [acá](https://github.com/pdepviernestm/2021-clases/blob/main/clase-02/correrTestsMasRapidoConGhcid.md).
