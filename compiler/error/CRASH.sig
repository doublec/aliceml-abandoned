(*
 * Handling of internal inconsistencies.
 *)


signature CRASH =
  sig

    exception CRASH of string

    val crash: string -> 'a

  end
