{
  if (match ($0, /^structure Tokens : Parser_TOKENS$/))
    savedline = $0;
  else {
    print $0;
    if (match ($0, /^structure ParserData:PARSER_DATA$/))
      print savedline;
  }
}
