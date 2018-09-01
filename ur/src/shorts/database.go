package shorts

import (
	"encoding/json"
	"errors"
	"os"
	"sync"
)

var (
	UnknownShortErr = errors.New("unknown short")
	DuplicateShortErr = errors.New("duplicate short")
)

// A database stores a mapping from shorts to redirects on disk. Manipulating
// a database from multiple goroutines through the same handle is safe.
type Database struct {
	mutex sync.Mutex
	path string
	entries map[string]string
}

// Open a database that already exists.
func Open(path string) (*Database, error) {
	file, err := os.Open(path)
	if err != nil { return nil, err }
	defer file.Close()

	var entries map[string]string
	err = json.NewDecoder(file).Decode(&entries)
	if err != nil { return nil, err }

	database := &Database{path: path, entries: entries}
	return database, nil
}

// Close a database. Call this once for every opened database.
func (d *Database) Close() {
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

// Insert a mapping from a short to a redirect into the database on disk. If
// the short already existed, return DuplicateShortErr.
func (d* Database) Insert(short, target string) error {
	d.mutex.Lock()
	defer d.mutex.Unlock()

	_, ok := d.entries[short]
	if ok { return DuplicateShortErr }

	d.entries[short] = target

	file, err := os.OpenFile(d.path, os.O_WRONLY | os.O_TRUNC, 0)
	if err != nil { return err }
	defer file.Close()

	return json.NewEncoder(file).Encode(d.entries)
}
