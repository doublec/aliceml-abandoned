structure Posix =
    struct
	structure ProcEnv =
	    struct
		type pid = int

		fun getpid () = 0
	    end
    end
