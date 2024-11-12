/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1998 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* The launcher for bytecode executables */

#define CAML_INTERNALS
#include "caml/exec.h"

#define STRICT
#define WIN32_LEAN_AND_MEAN

#include <windows.h>

/* C11's _Noreturn is deprecated in C23 in favour of attributes */
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202311L
  #define NORETURN [[noreturn]]
#else
  #define NORETURN _Noreturn
#endif

#if WINDOWS_UNICODE
#define CP CP_UTF8
#else
#define CP CP_ACP
#endif

#define SEEK_END FILE_END

#define lseek(h, offset, origin) SetFilePointer((h), (offset), NULL, (origin))

static int read(HANDLE h, LPVOID buffer, DWORD buffer_size)
{
  DWORD nread = 0;
  ReadFile(h, buffer, buffer_size, &nread, NULL);
  return nread;
}

static BOOL WINAPI ctrl_handler(DWORD event)
{
  if (event == CTRL_C_EVENT || event == CTRL_BREAK_EVENT)
    return TRUE;                /* pretend we've handled them */
  else
    return FALSE;
}

static void write_error(const wchar_t *wstr, HANDLE hOut)
{
  DWORD consoleMode, numwritten, len;
  char str[MAX_PATH];

  if (GetConsoleMode(hOut, &consoleMode) != 0) {
    /* The output stream is a Console */
    WriteConsole(hOut, wstr, lstrlen(wstr), &numwritten, NULL);
  } else { /* The output stream is redirected */
    len =
      WideCharToMultiByte(CP, 0, wstr, lstrlen(wstr), str, sizeof(str),
                          NULL, NULL);
    WriteFile(hOut, str, len, &numwritten, NULL);
  }
}

NORETURN static void exit_with_error(const wchar_t *wstr1,
                                     const wchar_t *wstr2,
                                     const wchar_t *wstr3)
{
  HANDLE hOut = GetStdHandle(STD_ERROR_HANDLE);
  if (wstr1) write_error(wstr1, hOut);
  if (wstr2) write_error(wstr2, hOut);
  if (wstr3) write_error(wstr3, hOut);
  write_error(L"\r\n", hOut);
  ExitProcess(2);
}

static uint32_t read_size(const char *ptr)
{
  const unsigned char *p = (const unsigned char *)ptr;
  return ((uint32_t) p[0] << 24) | ((uint32_t) p[1] << 16) |
         ((uint32_t) p[2] << 8) | p[3];
}

static char * read_runtime_path(HANDLE fd)
{
  char buffer[TRAILER_SIZE];
  static char runtime_path[MAX_PATH];
  int num_sections;
  uint32_t path_size;
  long ofs;

  if (lseek(fd, -TRAILER_SIZE, SEEK_END) == -1) return NULL;
  if (read(fd, buffer, TRAILER_SIZE) < TRAILER_SIZE) return NULL;
  num_sections = read_size(buffer);
  ofs = TRAILER_SIZE + num_sections * 8;
  if (lseek(fd, -ofs, SEEK_END) == -1) return NULL;
  path_size = 0;
  for (int i = 0; i < num_sections; i++) {
    if (read(fd, buffer, 8) < 8) return NULL;
    if (buffer[0] == 'R' && buffer[1] == 'N' &&
        buffer[2] == 'T' && buffer[3] == 'M') {
      path_size = read_size(buffer + 4);
      ofs += path_size;
    } else if (path_size > 0)
      ofs += read_size(buffer + 4);
  }
  if (path_size == 0) return NULL;
  if (path_size >= MAX_PATH) return NULL;
  if (lseek(fd, -ofs, SEEK_END) == -1) return NULL;
  if (read(fd, runtime_path, path_size) != path_size) return NULL;

  return runtime_path;
}

NORETURN void __cdecl wmainCRTStartup(void)
{
  wchar_t truename[MAX_PATH];
  char *runtime_path;
  wchar_t wruntime_path[MAX_PATH];
  HANDLE h;
  STARTUPINFO stinfo;
  PROCESS_INFORMATION procinfo;
  DWORD retcode;

  if (GetModuleFileName(NULL, truename, sizeof(truename)/sizeof(wchar_t)) == 0)
    exit_with_error(L"Out of memory", NULL, NULL);

  h = CreateFile(truename, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                 NULL, OPEN_EXISTING, 0, NULL);
  if (h == INVALID_HANDLE_VALUE ||
      (runtime_path = read_runtime_path(h)) == NULL ||
      !MultiByteToWideChar(CP, 0, runtime_path, -1, wruntime_path,
                           sizeof(wruntime_path)/sizeof(wchar_t)))
    exit_with_error(NULL, truename,
                    L" not found or is not a bytecode executable file");
  CloseHandle(h);
  if (SearchPath(NULL, wruntime_path, L".exe", sizeof(truename)/sizeof(wchar_t),
                 truename, NULL)) {
    /* Need to ignore ctrl-C and ctrl-break, otherwise we'll die and take
       the underlying OCaml program with us! */
    SetConsoleCtrlHandler(ctrl_handler, TRUE);

    stinfo.cb = sizeof(stinfo);
    stinfo.lpReserved = NULL;
    stinfo.lpDesktop = NULL;
    stinfo.lpTitle = NULL;
    stinfo.dwFlags = 0;
    stinfo.cbReserved2 = 0;
    stinfo.lpReserved2 = NULL;
    if (CreateProcess(truename, GetCommandLine(), NULL, NULL, TRUE, 0,
                      NULL, NULL, &stinfo, &procinfo)) {
      CloseHandle(procinfo.hThread);
      WaitForSingleObject(procinfo.hProcess, INFINITE);
      GetExitCodeProcess(procinfo.hProcess, &retcode);
      CloseHandle(procinfo.hProcess);
      ExitProcess(retcode);
    }
  }

  exit_with_error(L"Cannot exec ", wruntime_path, NULL);
}
