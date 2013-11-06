signature TOKENS = sig type t end

signature DATA =
  sig
    structure Token : TOKENS
  end

signature LRVALS =
  sig
    structure Tokens : TOKENS
    structure Data : DATA
    sharing type Data.Token.t = Tokens.t
  end


signature TOKEN =
  sig
    structure LrTable : sig end
  end

signature PARSER =
  sig
     structure LrTable : sig end
     structure Token : TOKEN
     sharing LrTable = Token.LrTable
  end
