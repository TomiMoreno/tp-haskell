{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Solucion where
-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

-- Retorna los nombres de los usuarios de la red sin repetidos
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios ([], _, _) = []
nombresDeUsuarios (usuario: usuarios, rs,ps) | pertenece usuario usuarios = nombresDeUsuarios (usuarios, rs, ps)
                                             | otherwise = nombreDeUsuario usuario : nombresDeUsuarios (usuarios, rs, ps)

-- Retorna los amigos de un usuario
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (_, [], _) _ = []
amigosDe (usuarios, (u1, u2):relaciones, publicaciones) user | sonElMismoUsuario user u1 = u2 : amigosDe (usuarios, relaciones, publicaciones) user
                                                             | sonElMismoUsuario user u2 = u1 : amigosDe (usuarios, relaciones, publicaciones) user
                                                             | otherwise = amigosDe (usuarios, relaciones, publicaciones) user

-- Retorna la cantidad de amigos del usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos (usuarios, relaciones, publicaciones) user = longitud (amigosDe (usuarios, relaciones, publicaciones) user)

-- Retorna el usuario con mas amigos
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([user], _, _) = user
usuarioConMasAmigos (user:usuarios, relaciones, publicaciones) | cantidadDeAmigos (usuarios, relaciones, publicaciones) user > cantidadDeAmigos (usuarios, relaciones, publicaciones) (head usuarios) = usuarioConMasAmigos (user:tail usuarios, relaciones, publicaciones)
                                                               | otherwise = usuarioConMasAmigos (usuarios, relaciones, publicaciones)

-- Retorna si existe un usuario con 1 millón de amigos o más
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([], _, _) = False
estaRobertoCarlos (user: usuarios, relaciones, publicaciones) | cantidadDeAmigos (user: usuarios, relaciones, publicaciones) user >= 1000000 = True
                                                              | otherwise = estaRobertoCarlos (usuarios, relaciones, publicaciones)

-- Retorna las publicaciones de un usuario
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_, _, []) _ = []
publicacionesDe (usuarios, relaciones, publicacion:publicaciones) user | user == (usuarioDePublicacion publicacion) = publicacion : siguientesPublicaciones
                                                                       | otherwise = siguientesPublicaciones 
                                                                       where siguientesPublicaciones = publicacionesDe (usuarios, relaciones, publicaciones) user

-- Retorna las publicaciones que le gustan a un usuario
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, []) _ = []
publicacionesQueLeGustanA (usuarios, relaciones, (author, texto, likes):publicaciones) user | pertenece user likes = (author, texto, likes) : siguientesPubs
                                                                                            | otherwise = siguientesPubs
                                                                                            where siguientesPubs = publicacionesQueLeGustanA (usuarios, relaciones, publicaciones) user


-- Retorna si a dos usuarios les gustan las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red user1 user2 = mismosElementos (publicacionesQueLeGustanA red user1) (publicacionesQueLeGustanA red user2)

-- Verifica si existe un usuario que le gusten todas las publicaciones de otro
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ([], _, _) _ = False
tieneUnSeguidorFiel red user | contiene (publicacionesDe red user) (publicacionesQueLeGustanA red usuarioActual) = True
                             | otherwise = tieneUnSeguidorFiel siguienteRed user
                             where
                              usuarioActual = head (usuarios red)
                              siguienteRed = (tail (usuarios red), relaciones red, publicaciones red)

-- Verifica si existe una secuencia de amigos entre 2 usuarios
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red usuarioInicio usuarioObjetivo = existeSecuenciaDeAmigosAux red [usuarioInicio] usuarioObjetivo []

-- Verifica si 
existeSecuenciaDeAmigosAux :: RedSocial -> [Usuario] -> Usuario -> [Usuario] -> Bool
existeSecuenciaDeAmigosAux _ [] _ _ = False
existeSecuenciaDeAmigosAux red (user:usuarios) usuarioObjetivo usuariosVisitados | pertenece usuarioObjetivo amigosActuales = True
                                                                                 | otherwise = existeSecuenciaDeAmigosAux red siguientesAmigos usuarioObjetivo (user:usuariosVisitados)
                                                                                  where
                                                                                    amigosActuales = user : amigosDe red user
                                                                                    siguientesAmigos = sacarTodos (usuarios ++ amigosActuales) usuariosVisitados


-- Funciones auxiliares

-- Verifica si un elemento pertenece a una lista
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e [] = False
pertenece e (x:xs) = e == x || pertenece e xs

-- Verifica si dos listas tienen los mismos elementos, sin importar el orden
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos [] l2 = False
mismosElementos l1 [] = False
mismosElementos (x:xs) l2 = pertenece x l2 && mismosElementos xs (sacarUnaVez x l2)

-- Verifica si la primer lista está contenida en la segunda
contiene :: (Eq t) => [t] -> [t] -> Bool
contiene [] [] = True
contiene [] l2 = True
contiene l1 [] = False
contiene (x:xs) l2 = pertenece x l2 && contiene xs l2

-- Remueve la primera ocurrencia del elemento dado
sacarUnaVez :: (Eq t) => t -> [t] -> [t]
sacarUnaVez e [] = []
sacarUnaVez e (x:xs) | e == x = xs
                     | otherwise = x : sacarUnaVez e xs

-- Remueve los elementos repetidos de una lista
quitarRepetidos :: (Eq t) => [t] -> [t]
quitarRepetidos [] = []
quitarRepetidos (x:xs) | pertenece x xs = quitarRepetidos xs
                       | otherwise = x : quitarRepetidos xs

-- Remueve todos los elementos de la primer lista que estén en la segunda
sacarTodos :: (Eq t) => [t] -> [t] -> [t]
sacarTodos [] l2 = []
sacarTodos l1 [] = l1
sacarTodos (x:xs) l2 | pertenece x l2 = sacarTodos xs l2
                     | otherwise = x : sacarTodos xs l2

-- Retorna la longitud de una lista
longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- Verifica que dos usuarios sean iguales
sonElMismoUsuario :: Usuario -> Usuario -> Bool
sonElMismoUsuario user1 user2 = idDeUsuario user1 == idDeUsuario user2

