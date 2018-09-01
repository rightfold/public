package shorts

import (
	"errors"
	"io"
	"os"
	"sync"
	"ur/encoding/mapping"
)

var (
	InvalidShortErr = errors.New("invalid short")
	UnknownShortErr = errors.New("unknown short")
)

// A database stores a mapping from shorts to redirects on disk. Manipulating
// a database from multiple goroutines through the same handle is safe.
type Database struct {
	mutex sync.Mutex
	file *os.File
	entries map[string]string
}

// Open a database that already exists.
func Open(path string) (*Database, error) {
	file, err := os.OpenFile(path, os.O_RDONLY, 0)
	if err != nil { return nil, err }

	entries, err := readDatabase(file)
	if err != nil {
		file.Close()
		return nil, err
	}

	return &Database{file: file, entries: entries}, nil
}

// Close a database. Call this once for every opened database.
func (d *Database) Close() {
	d.file.Close()
}

// Look up a short in the database and return the redirect. If it does not
// exist, return UnknownShortErr.
func (d* Database) Lookup(short string) (redirect string, err error) {
	d.mutex.Lock()
	defer d.mutex.Unlock()

	redirect, ok := d.entries[short]
	if !ok { err = UnknownShortErr }
	return
}

func readDatabase(r io.Reader) (map[string]string, error) {
	mr := mapping.NewReader(r)
	entries := make(map[string]string)
	for mr.Scan() {
		short, redirect, err := mr.Mapping()
		if err != nil { return nil, err }
		entries[short] = redirect
	}
	if err := mr.Err(); err != nil { return nil, err }
	return entries, nil
}
