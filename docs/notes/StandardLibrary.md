
## File System

Possible way forward:

1. make an export sealed interface FileError (vale's equivalent of enum)

2. make an export struct and impl it (vale's equivalent of an enum case):

```
export struct NotFound { }
impl FileError for NotFound;
```

3. of course, we want to return a Result not an error... so lets export Result:

```
export Result<i64, FileError> as FileResult;
export Result<i64, FileError> as StringResult;
```

If that works, then we should be able to do some simple file operations, similar to this Rust snippet:

```
let mut file = File::open(&path).expect("Error opening File");
let mut contents = String::new();
file.read_to_string(&mut contents).expect("Unable to read to string");
```

in Vale it might be:

```
file = FileOpen("myfile.txt").Expect("Error opening File");
contents = file.ReadToString().Expect("Unable to read to string");

string_to_write = “asd”;
file.WriteString(string_to_write).Expect("Unable to write to file");
```

we'd need these:

```
extern func FileOpen(path str) FileResult;

extern func ReadToString(file i64) StringResult;

extern func WriteString(file i64, contents str) StringResult;
```

For now we can communicate with Rust in terms of that i64. After that works, as a later step we can add a real vale File struct to wrap it (that part would be just a wrapper around the above functions; we'll still use the above functions under the hood)

This is better than what we roughly had in path.vale because:
It communicates Results across the FFI boundary, which makes things a lot better. This way, we can convey errors pretty seamlessly from Rust into Vale. Right now we just kinda toss an int over the boundary and ignore it lol
It uses the Rust standard library under the hood, which is more reliable than the C code we were writing in the stdlib

```
cargo init FSVale --lib
```

add this to cargo toml:

```
[lib]
crate-type = ["staticlib"]

and then put this into lib.rs:

use std::fs::{self, File};

use std::ffi::CStr;
use std::os::raw::c_char;

#[no_mangle]
pub extern "C" fn createDir(path: *const c_char) -> usize {
  let slice = unsafe { CStr::from_ptr(path) };
  let create_dir_result = fs::create_dir(slice.to_str().unwrap());
  match create_dir_result {
    Ok(result) => 0,
    Err(error) => 1,
  }
}
```

That will generate a library in the target directory.

This C can call into it:

```
extern void createDir(char *p);
int main(void) {
  createDir("Created from Rust stdlib!");
  return 0;
}
```

Use `-lFSVale -L target\debug`
