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
publicacionesDe (usuarios, relaciones, publicacion:publicaciones) user | sonElMismoUsuario user (usuarioDePublicacion publicacion) = publicacion : publicacionesDe (usuarios, relaciones, publicaciones) user
                                                                       | otherwise = publicacionesDe (usuarios, relaciones, publicaciones) user

-- Retorna las publicaciones que le gustan a un usuario
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, []) _ = []
publicacionesQueLeGustanA (usuarios, relaciones, (author, texto, likes):publicaciones) user | pertenece user likes = (author, texto, likes) : publicacionesQueLeGustanA (usuarios, relaciones, publicaciones) user
                                                                                            | otherwise = publicacionesQueLeGustanA (usuarios, relaciones, publicaciones) user

-- Retorna si a dos usuarios les gustan las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red user1 user2 = mismosElementos (publicacionesQueLeGustanA red user1) (publicacionesQueLeGustanA red user2)

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined

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

-- Remueve la primera ocurrencia del elemento dado
sacarUnaVez :: (Eq t) => t -> [t] -> [t]
sacarUnaVez e [] = []
sacarUnaVez e (x:xs) | e == x = xs
                     | otherwise = x : sacarUnaVez e xs

-- Retorna la longitud de una lista
longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- Verifica que dos usuarios sean iguales
sonElMismoUsuario :: Usuario -> Usuario -> Bool
sonElMismoUsuario user1 user2 = idDeUsuario user1 == idDeUsuario user2

