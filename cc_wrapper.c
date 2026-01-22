#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <process.h>

int main(int argc, char **argv) {
    char **new_argv = malloc((argc + 1) * sizeof(char *));
    int new_argc = 0;
    
    // clang.exe のパス (本物)
    new_argv[new_argc++] = "C:\\Users\\Cocoa\\scoop\\apps\\llvm\\current\\bin\\clang.exe";
    
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-fPIC") == 0) {
            continue; // -fPIC を無視
        }
        new_argv[new_argc++] = argv[i];
    }
    new_argv[new_argc] = NULL;
    
    // 本物の clang を呼び出す
    return _spawnv(_P_WAIT, new_argv[0], (const char * const *)new_argv);
}

