(*
 * Handling of internal inconsistencies.
 *)


signature CRASH =
  sig

    exception Crash of string

    val crash: string -> 'a

  end
