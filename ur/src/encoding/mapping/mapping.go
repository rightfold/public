package mapping

import (
	"bufio"
	"errors"
	"io"
	"strings"
)

var ParseErr = errors.New("parse error")

type Reader struct {
	*bufio.Scanner
}

func NewReader(r io.Reader) (*Reader) {
	return &Reader{Scanner: bufio.NewScanner(r)}
}

func (r *Reader) Mapping() (string, string, error) {
	line := r.Text()
	parts := strings.SplitN(line, " ", 2)
	if len(parts) != 2 { return "", "", ParseErr }
	return parts[0], parts[1], nil
}
