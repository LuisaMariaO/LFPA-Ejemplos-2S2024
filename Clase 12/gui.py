import tkinter as tk
from tkinter import ttk
import subprocess


def analizar():

    # Limpiar todos los elementos del Treeview
    for item in tree.get_children():
        tree.delete(item)
   

    # Obtener el contenido del área de texto izquierda
    data = text_area.get("1.0", tk.END)
    
    
    # Compilar y ejecutar el código Fortran
    comando = subprocess.run(
        ["gfortran", "-o", "main.exe", "main.f90"],  # compilación de Fortran
        check=True  # detener si hay un error en la compilación
    )
    resultado = subprocess.run(
        ["./main.exe"],  # Ejecutable de Fortran
        input=data,  # la data que se manda a Fortran
        stdout=subprocess.PIPE,  # la data que viene de Fortran   
        text=True  # la salida se maneja como texto
    )
   
    salida_fortran = resultado.stdout.strip()  # Obtiene la salida y elimina espacios extra
 
    for line in salida_fortran.splitlines():
            print(line)
            # Separar los atributos utilizando el espacio como delimitador
            partes = line.split('|')  # Esto separa los atributos según el espacio
            if len(partes) == 5:
                tipo = partes[0]
                linea = partes[1].lstrip()
                columna = partes[2].lstrip()
                token = partes[3]
                descripcion = ' '.join(partes[4:])  # Junta el resto como descripción
                # Insertar en la tabla
                tree.insert('', tk.END, values=(tipo, linea, columna, token, descripcion))
    # Insertar la nueva salida en el área de texto derecha
    #right_text_area.insert(tk.END, resultado.stdout)

    text_area.delete("1.0", tk.END)  # Limpiar el área de texto izquierda

# Crear la ventana principal
root = tk.Tk()
root.title("Interfaz de Análisis")
root.geometry("1500x700")  # Establece el tamaño de la ventana

# Crear el menú superior
menu_bar = tk.Menu(root)
root.config(menu=menu_bar)

# Agregar opciones al menú
archivo_menu = tk.Menu(menu_bar, tearoff=0)
menu_bar.add_cascade(label="Archivo", menu=archivo_menu)

analisis_menu = tk.Menu(menu_bar, tearoff=0)
menu_bar.add_cascade(label="Análisis", menu=analisis_menu)
analisis_menu.add_command(label="Analizar", command=analizar)

tokens_menu = tk.Menu(menu_bar, tearoff=0)
menu_bar.add_cascade(label="Tokens", menu=tokens_menu)

# Usar grid para una mejor organización del layout
root.grid_rowconfigure(0, weight=3)  # Para que el text area ocupe 60% del espacio
root.grid_rowconfigure(1, weight=2)  # Para que la tabla ocupe el 40%
root.grid_columnconfigure(0, weight=1)

# Text area que ocupa el 60% del alto de la ventana
text_area = tk.Text(root, wrap=tk.WORD)
text_area.grid(row=0, column=0, sticky='nsew', padx=10, pady=10)

# Frame para la tabla que ocupará el 40% restante
frame_tabla = tk.Frame(root)
frame_tabla.grid(row=1, column=0, sticky='nsew', padx=10, pady=10)

# Crear la tabla (Treeview)
columns = ('tipo', 'linea', 'columna', 'token', 'descripcion')
tree = ttk.Treeview(frame_tabla, columns=columns, show='headings')

# Definir los encabezados de la tabla
tree.heading('tipo', text='Tipo')
tree.heading('linea', text='Línea')
tree.heading('columna', text='Columna')
tree.heading('token', text='Token')
tree.heading('descripcion', text='Descripción')

# Ajustar las columnas para que ocupen todo el ancho
for col in columns:
    tree.column(col, width=100, anchor=tk.W)

# Agregar barra de desplazamiento vertical para la tabla
scrollbar = ttk.Scrollbar(frame_tabla, orient=tk.VERTICAL, command=tree.yview)
tree.configure(yscroll=scrollbar.set)
scrollbar.pack(side=tk.RIGHT, fill=tk.Y)

# Empacar la tabla en el frame
tree.pack(fill=tk.BOTH, expand=True)



# Ejecutar la aplicación
root.mainloop()