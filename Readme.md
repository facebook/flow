
Facebook
/
flujo
Público
Agrega escritura estática a JavaScript para mejorar la productividad del desarrollador y la calidad del código.

flujo.org/
Licencia
 licencia MIT
 21,9k estrellas 1,9k horquillas 
Código
Cuestiones
2.2k
Solicitudes de extracción
92
Comportamiento
Proyectos
1
wiki
Seguridad
Perspectivas
facebook/flujo
Última confirmación
@r-barnes
@facebook-github-bot
r-barnes y facebook-github-bot
…
hace 2 días
Estadísticas de Git
archivos
LÉAME.md
Flujo
CírculoCI Sigue a @flowtype Licencia MIT Colaboradores de GitHub Idioma principal de GitHub Únete al chat de Discord

Flow es un verificador de tipos estático para JavaScript. Para obtener más información sobre Flow, consulte flow.org .

Para obtener más información general sobre el proyecto, lea este resumen .

Contenido
Requisitos
Uso de flujo
Usando el analizador de Flow desde JavaScript
Flujo de construcción desde la fuente
Únete a la comunidad Flow
Licencia
Requisitos
El flujo funciona con:

macOS (x86_64)
Linux (x86_64 y arm64)
Windows (x86_64, se recomienda Windows 10)
Hay distribuciones binarias para cada una de estas plataformas y también puede compilarlas desde la fuente en cualquiera de ellas.

Uso de flujo
Consulte las instrucciones de instalación y luego los documentos de uso .

Usando el analizador de Flow desde JavaScript
Si bien Flow está escrito en OCaml, su analizador está disponible como un módulo compilado en JavaScript publicado en npm, llamado flow-parser . La mayoría de los usuarios finales de Flow no necesitarán usar este analizador directamente , pero los paquetes de JavaScript que utilizan el análisis sintáctico de JavaScript tipo Flow pueden usar esto para generar el árbol de sintaxis de Flow con tipos anotados adjuntos.

Flujo de construcción desde la fuente
El flujo está escrito en OCaml (se requiere OCaml 4.14.0).

Instalar dependencias del sistema:

Mac:brew install opam

Debian:sudo apt-get install opam

Otro Linux: ver documentos opam

Windows: se requieren cygwin y una serie de dependencias como make, gccy g++.

Una forma de instalar todo es instalar Chocolatey y luego ejecutar .\scripts\windows\install_deps.ps1y .\scripts\windows\install_opam.ps1. De lo contrario, consulte la sección "Instalación manual" de los documentos de OCaml para Windows e instale todos los paquetes enumerados en nuestro archivo install_deps.ps1.

El resto de estas instrucciones debe ejecutarse dentro del shell de Cygwin: C:\tools\cygwin\Cygwin. entonces cd /cygdrive/c/Users/you/path/to/checkout_

Validar la opamversión es 2.x.x:

opam --version
Las siguientes instrucciones esperan 2.x.x. Si su administrador de paquetes ha instalado una 1.x.xversión, consulte los documentos de opam para instalar una versión más nueva manualmente.

Inicializar opam:

# on Mac and Linux:
opam init

# on Windows:
scripts/windows/init_opam.sh
Instale las dependencias OCaml de Flow:

# from within this git checkout
make deps
nota : si encuentra que obtiene un error al buscar la ocaml-base-compilerversión, su repositorio de dependencia local puede estar desactualizado y necesita ejecutar opam update+opam upgrade

Construye el flowbinario:

eval $(opam env)
make
Esto produce el bin/flowbinario.

Construir flow.js(opcional):

opam install -y js_of_ocaml.4.0.0
make js
Esto produce bin/flow.js.

El analizador de flujo también se puede compilar en JavaScript. Lea cómo aquí .

Ejecutando las pruebas
Para ejecutar las pruebas, primero compile el flujo usando make. Entonces correbash ./runtests.sh bin/flow

Hay un make testobjetivo que compila y ejecuta pruebas.

Para ejecutar un subconjunto de las pruebas, puede pasar un segundo argumento al runtests.sharchivo.

Por ejemplo:bash runtests.sh bin/flow class | grep -v 'SKIP'

Únete a la comunidad Flow
Sitio web: https://flow.org
Discordia: https://discord.gg/8ezwRUK
irc: #tipo de flujo en Freenode
Twitter: siga @flowtype y #flowtype para mantenerse al día con las últimas noticias de Flow.
Desbordamiento de pila: haga una pregunta con la etiqueta de tipo de flujo
Licencia
Flow tiene licencia MIT ( LICENCIA ). El sitio web y la documentación están autorizados bajo la licencia Creative Commons Attribution 4.0 ( sitio web/LICENCIA-DOCUMENTACIÓN ).

Lanzamientos 276
v0.196.3: corrección de pruebas de servicio de acción de código para leer datos comprometidos
El último
Hace 4 días
+ 275 lanzamientos
