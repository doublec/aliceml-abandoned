BEGIN {
   import=0;
   FS="\n";
}

{
   if (!import) {
      if (match($0, /^import[^"]*"[^"]*".*$/)) {
         print "";
      }
      else if (match($0, /^import[^"]*$/)) {
         import = 1;
         print "";
      }
      else {
         print $0;
      }
   }
   else {
      print "";
      if (match($0, /^[^"]*"[^"]*".*$/)) {
	 import = 0;
      }
   }
}
