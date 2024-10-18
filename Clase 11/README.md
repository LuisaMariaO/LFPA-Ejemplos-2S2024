# Pasos sugereidos para terminar la traducción
1. Agregar un atributo al control para poder almacenar los controles hijo que se le pueden asignar con .add
~~~
type :: Control
        character(len=:), allocatable :: tipo
        character(len=:), allocatable :: identificador
        character(len=:), allocatable :: texto
        character(len=:), allocatable :: alineacion
        character(len=:), allocatable :: marcado
        character(len=:), allocatable :: grupo
        character(len=:), allocatable :: colorLetra
        character(len=:), allocatable :: colorFondo
        character(len=:), allocatable :: ancho
        character(len=:), allocatable :: alto
        character(len=:), allocatable :: html_apertura
        character(len=:), allocatable :: html_cierre !SOLO USARLO SI SON CONTENEDORES, DE LO CONTRARIO LLENAR SOLO LA DE APERTURA PARA EVITAR PROBLEMAS CON RECURSIVIDAD
        character(len=:), allocatable :: x
        character(len=:), allocatable :: y
        type(control), allocatable :: hijos(:) !ARREGLO CON HIJOS QUE SE LE PUEDEN ASIGNAR CON .ADD
        
~~~~

2. Agregar un componente inicial que guarde el inicio y fin del html resultante.

~~~
type(Control) :: controlInicial
!Para el control inicial
character(len=:), allocatable :: tipo
character(len=:), allocatable :: id

tipo="this"
id="this"
        

!CREACION DE EL CONTROLADOR PRINCIPAL QUE CONTIENE LA BASE HTML
call this%agregarControl(tipo, id)
!Obtengo el controloador que acabo de crear y le asigno el html inicial y final       
controlInicial%html_apertura = '<html><head><link href="estilos.css" rel="stylesheet"type="text/css" /></head><body>'  
      
controlInicial%html_cierre = controlInicial%html_cierre // '</body></html>'

~~~

**Nota:** El archivo css que generen se debe de llamar como indiquen en el encabezado, en este caso, estilos.css

2.Al terminar el reconocimiento de propiedades, pueden implementar un método para traducir, en donde deben:
- Iterar los controles que tengan en la lista
- Verificar de que tipo son y generar el html de apertura y cierre según la página 13 del enunciado. Guardarlo en cada control.

3. Terminar su gramáticia para reconocer el posicionamiento en el bloque de Colocación, agregar los atributos reconocidos al control, tal y como se agregan propiedades.

4. Si en colocación se encuentran con un "control_padre.add(control_hijo)".
- Buscar control_padre en la lista
- Buscar control_hijo en la lista
- Añadir control_hijo a la lista de hijos de control_padre

Esta es una forma de agregar un hijo a un padre, hay más formas:
~~~
 ! Redimensionar el array de hijos para añadir el nuevo hijo
    if (.not. allocated(control_padre%hijos)) then
      allocate(control_padre%hijos(1))
      control_padre%hijos(1) = control_hijo
    else
      control_padre%hijos = [control_padre%hijos, control_hijo]
    endif
  end subroutine add
~~~

5. Por último, al final de todos los bloques, hacer una función recursiva que reciba como parámetro el control principal (this, el de la posición 1) como parámetro, cuando se llame la función por primera vez.
- Escribir el html de apertura
- Si el control tiene hijos, recorrer el array de hijos y que se llame a sí misma con cada hijo
- Escriba el html final del control
- Acá además, se verifica en las propiedades de cada control si tienen asignadas propiedades de css (ancho, alto, color, etc...) si las tienen, escribir también en una variable o archivo para css.
**Nota:**
El contenido tanto html como css puede ser escrito en un string y escribirlo después en el archivo o escribirlo directamente.

6. Al final escribir los archivos con el contenido traducido.