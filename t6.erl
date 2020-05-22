% registra_asistente(Clave, Nombre)               
% elimina_asistente(Asistente)                    
% inscribe_conferencia(Asistente, Conferencia)    
% desinscribe_conferencia(Asistente, Conferencia) 
% registra_conferencia(Clave, Titulo, Cupo)       
% elimina_conferencia(Conferencia)                
% asistentes_inscritos(Conferencia)               
% conferencias_inscritas(Asistente)   
% lista_asistentes()                              
% lista_conferencias()

% ———————————— ESTRUCTURAS DE DATOS ——————————————————


%  Conferencia: Tupla con {ClaveConferencia, Titulo, Cupo, [ClaveAsistentes]}
%  Asistentes: Lista de Tuplas con  {Clave, Nombre}
%  Conferencias: Lista de Claves de conferencias

% ——————————— —————————————————— ——————————————————


-module(t6).
-import(lists, [filter/2, foreach/2]).
-export([
  admin/2, inicia_admin/0, termina_admin/0, 
  registra_asistente/2, elimina_asistente/1,
  registra_conferencia/3, elimina_conferencia/1,
  muestra_asistente/2,
  conferencia/4, buscar_asistente/2,
  admin_elimina_asistente/2, buscar_conferencia/2, buscar_asistente_conferencia/2,
  admin_elimina_conferencia/2, inscribe_conferencia/2, 
  desinscribe_conferencia/2, muestra_conferencia_asistente/2,
  lista_asistentes/0, lista_conferencias/0
]).

buscar_asistente(_, []) -> no_existe;
buscar_asistente(Clave, [{Clave, _} | _]) -> Clave;
buscar_asistente(Clave, [_ | T]) -> buscar_asistente(Clave, T).

admin_elimina_asistente(_, []) -> [];
admin_elimina_asistente(Clave, [{Clave, _} | T]) -> T;
admin_elimina_asistente(Clave, [Asistente | T]) ->
  [Asistente] ++ admin_elimina_asistente(Clave, T).
  
buscar_conferencia(_, []) -> no_existe;
buscar_conferencia(Clave, [Clave | _]) -> Clave;
buscar_conferencia(Clave, [_ | T]) -> buscar_conferencia(Clave, T).

admin_elimina_conferencia(Clave, Conferencias) ->
  lists:filter(
    fun({EstaClave, _, _}) -> EstaClave =/= Clave end, Conferencias
  ).

% Conferencia: Tupla con {ClaveConferencia, Titulo, Cupo, [ClaveAsistentes]}
conferencia(Clave, Titulo, Cupo, Asistentes) ->
  receive
    {lista_asistentes, ClaveAsistente} -> 
      administracion ! {conferencia_asistente,Titulo, Asistentes, ClaveAsistente},
      conferencia(Clave, Titulo, Cupo, Asistentes);
    {PID, inscribe, ClaveAsistente} when length(Asistentes) < Cupo ->
      PID ! {"Inscrito en conferencia", Clave},
      conferencia(Clave, Titulo, Cupo, Asistentes ++ [ClaveAsistente]);
    {PID, inscribe, _} when length(Asistentes) >= Cupo ->
      PID ! {"No hay cupo", Clave},
      conferencia(Clave, Titulo, Cupo, Asistentes);
    {desinscribe, ClaveAsistente} ->
      conferencia(Clave, Titulo, Cupo, lists:delete(ClaveAsistente, Asistentes));
    {muestra} ->
      muestra_conferencia(Titulo, Cupo, Asistentes),
      conferencia(Clave, Titulo, Cupo, Asistentes)
  end.


muestra_asistente({Clave, Nombre}, Conferencias) ->
  io:format("Asistente ~p - ~p~n --Conferencias: ", [Clave,Nombre]),
  lists:foreach(fun(Conferencia) -> muestra_conferencia_asistente(Conferencia, Clave) end, Conferencias).

 muestra_conferencia(Titulo, Cupo, Asistentes) ->
    io:format(" Conferencia: ~p ~n Cupo: ~p ~n Asistentes: ~n", [Titulo, Cupo]),
    lists:foreach(fun(Asistente) ->  io:format(" ~p~n", [Asistente]) end, Asistentes).

muestra_conferencia_asistente(Conferencia, Clave) ->
  Conferencia ! {lista_asistentes, Clave}.

buscar_asistente_conferencia(_, []) -> no_existe;
buscar_asistente_conferencia(Clave, [Clave | _]) -> Clave;
buscar_asistente_conferencia(Clave, [_ | T]) -> buscar_asistente_conferencia(Clave, T).

%  Conferencia: Tupla con {ClaveConferencia, Titulo, Cupo, [ClaveAsistentes]}
%  Asistentes: Lista de Tuplas con  {Clave, Nombre}
%  Conferencias: Lista de Claves de conferencias
admin(Asistentes, Conferencias) ->
  process_flag(trap_exit, true),
  receive
    {muestra_asistentes} ->
      lists:foreach(fun(Asistente) -> muestra_asistente(Asistente, Conferencias) end, Asistentes),
      admin(Asistentes, Conferencias);
    {conferencia_asistente, Titulo, AsistentesConferencia, ClaveAsistente} -> 
      case buscar_asistente_conferencia(ClaveAsistente, AsistentesConferencia) of
        no_existe ->
          io:format("",[]),
          admin(Asistentes, Conferencias);
        _ ->
          io:format(" ~p ", [Titulo]),
          admin(Asistentes, Conferencias)
      end;

    {muestra_conferencias} ->
      lists:foreach(fun(Conferencia) -> Conferencia ! {muestra} end, Conferencias),
      admin(Asistentes, Conferencias);
    {PID, registra_asistente, Clave, Nombre} ->
      case buscar_asistente(Clave, Asistentes) of
        no_existe ->
          io:format("El asistente '~p' fue agregado.~n", [Clave]),
          PID ! {"El asistente fue agregado", Clave},
          admin(Asistentes ++ [{Clave, Nombre}], Conferencias);
        _ ->
          io:format("Ya existe un asistente con clave '~p'.~n", [Clave]),
          PID ! {"El asistente ya existe", Clave},
          admin(Asistentes, Conferencias)
      end;
    {PID, elimina_asistente, Clave} ->
      case buscar_asistente(Clave, Asistentes) of
        no_existe ->
          io:format("No existe asistente con clave '~p'.~n", [Clave]),
          PID ! {"El asistente no existe", Clave},
          admin(Asistentes, Conferencias);
        _ ->
          io:format("El asistente '~p' fue eliminado.~n", [Clave]),
          PID ! {"El asistente fue eliminado", Clave},
          admin(admin_elimina_asistente(Clave,Asistentes), Conferencias)
      end;
    {PID, registra_conferencia, Clave, Titulo, Cupo} ->
      case buscar_conferencia(Clave, Conferencias) of
        no_existe ->
          io:format("La conferencia '~p' fue agregada.~n", [Clave]),
          register(Clave, spawn(t6, conferencia, [Clave, Titulo, Cupo, []])),
          PID ! {"La conferencia fue agregada", Clave},
          admin(Asistentes, Conferencias ++ [Clave]);
        _ ->
          io:format("Ya existe una conferencia con clave '~p'.~n", [Clave]),
          PID ! {"La conferencia ya existe", Clave},
          admin(Asistentes, Conferencias)
      end;
    {PID, elimina_conferencia, Clave} ->
      case buscar_conferencia(Clave, Conferencias) of
        no_existe ->
          io:format("No existe conferencia con clave '~p'.~n", [Clave]),
          PID ! {"La conferencia no existe", Clave},
          admin(Asistentes, Conferencias);
        _ ->
          io:format("Conferencia '~p' fue eliminada.~n", [Clave]),
          exit(whereis(Clave), normal),
          unregister(Clave),
          PID ! {"La conferencia fue eliminada", Clave},
          admin(Asistentes, admin_elimina_conferencia(Clave, Conferencias))
      end;
    {PID, inscribe_conferencia, ClaveAsistente, ClaveConferencia} ->
      case buscar_conferencia(ClaveConferencia, Conferencias) of
        no_existe ->
          io:format("No existe conferencia con clave '~p'.~n", [ClaveConferencia]),
          PID ! no_existe_conferencia,
          admin(Asistentes, Conferencias);
        _ ->
          case buscar_asistente(ClaveAsistente, Asistentes) of
            no_existe ->
              io:format("No existe asistente con clave '~p'.~n", [ClaveAsistente]),
              PID ! no_existe_asistente,
              admin(Asistentes, Conferencias);
            _ ->
              io:format("El asistente con clave '~p' se logro inscribir a '~p' ~n", [ClaveAsistente, ClaveConferencia]),
              ClaveConferencia ! {PID, inscribe, ClaveAsistente},
              admin(Asistentes, Conferencias)
          end
      end;
    {PID, desinscribe_conferencia, ClaveAsistente, ClaveConferencia} ->
      case buscar_conferencia(ClaveConferencia, Conferencias) of
        no_existe ->
          io:format("No existe conferencia con clave '~p'.~n", [ClaveConferencia]),
          PID ! no_existe_conferencia,
          admin(Asistentes, Conferencias);
        _ ->
          case buscar_asistente(ClaveAsistente, Asistentes) of
            no_existe ->
              io:format("No existe asistente con clave '~p' . ~n", [ClaveAsistente]),
              PID ! no_existe_asistente,
              admin(Asistentes, Conferencias);
            _ ->
              ClaveConferencia ! {PID, desinscribe, ClaveAsistente},
              admin(Asistentes, Conferencias)
          end
      end   
  end.

inicia_admin() ->
  io:format("Servidor de administracion corriendo ~n"),
  register(administracion, spawn(t6, admin, [[],[]])).

termina_admin() ->
  exit(whereis(administracion), normal),
  unregister(administracion),
  io:format("Servidor de administracion apagado~n").

% CODIGO PARA EL CLIENTE

% nombre corto del servidor (nombre@máquina)
nodo_servidor() -> 'administracion@DESKTOP-VC6T7V2'.

registra_asistente(Clave, Nombre) ->
  Nodo = nodo_servidor(),
  {administracion, Nodo} ! {self(), registra_asistente, Clave, Nombre},
  receive
    {Mensaje, Clave} -> io:format("~p: ~p~n", [Mensaje, Clave])
  end.

elimina_asistente(Clave) ->
  Nodo = nodo_servidor(),
  {administracion, Nodo} ! {self(), elimina_asistente, Clave},
  receive
    {Mensaje, Clave} -> io:format("~p: ~p~n", [Mensaje, Clave])
  end.

registra_conferencia(Clave, Titulo, Cupo) ->
  Nodo = nodo_servidor(),
  {administracion, Nodo} ! {self(), registra_conferencia, Clave, Titulo, Cupo},
  receive
    {Mensaje, Clave} -> io:format("~p: ~p~n", [Mensaje, Clave])
  end.

elimina_conferencia(Clave) ->
  Nodo = nodo_servidor(),
  {administracion, Nodo} ! {self(), elimina_conferencia, Clave},
  receive
    {Mensaje, Clave} -> io:format("~p: ~p~n", [Mensaje, Clave])
  end.

inscribe_conferencia(Asistente, Conferencia) ->
  Nodo = nodo_servidor(),
  {administracion, Nodo} ! {self(), inscribe_conferencia, Asistente, Conferencia},
  receive
    {Mensaje, Clave} -> io:format("~p: ~p~n", [Mensaje, Clave])
  end.

desinscribe_conferencia(Asistente, Conferencia) ->
  Nodo = nodo_servidor(),
  {administracion, Nodo} ! {self(), elimina_conferencia, Asistente, Conferencia},
  receive
    {Mensaje, Clave} -> io:format("~p: ~p~n", [Mensaje, Clave])
  end.

lista_conferencias() ->
  administracion ! {muestra_conferencias},
  ok.
  
lista_asistentes() -> 
  administracion ! {muestra_asistentes},
  io:format("",[]).
