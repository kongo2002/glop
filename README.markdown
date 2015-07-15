
# glop

*glop* is a small log file analyzer for the [gentoo][gentoo] portage emerge log.


## Installation

The setup is pretty straightforward for a cabal based package:

    $ git clone git://github.com/kongo2002/glop.git
    $ cd glop
    $ cabal install


### Cabal sandbox

Feel free to alternatively install the executable inside a cabal sandbox:

    $ git clone git://github.com/kongo2002/glop.git
    $ cd glop
    $ cabal sandbox init
    $ cabal install --dependencies-only
    $ cabal build

The resulting executable can then be found at `./dist/build/glop/glop`.


## Usage

~~~
Usage: glop [OPTIONS] [PACKAGE...]

You may specify PACKAGE in one of three forms:
  nginx              search for package named 'nginx'
  www-servers/       search for packages in 'www-servers'
  www-servers/nginx  search for exact match 'www-servers/nginx'

Apart from specifying the emerge log file via the -f switch
you may supply the log contents via STDIN by appending a dash:
  zcat /tmp/emerge.log.gz | glop -

  -f FILE  --file=FILE  emerge log file
  -c                    show current emerge's progress
  -l                    display last emerged packages
  -s                    display rsync operations
  -V       --version    print version
  -h       --help       show this help
~~~


## Examples

### Specific package

When searching for a specific package you may omit the category:

~~~
# basically the same as: glop dev-haskell/cabal
$ glop cabal
dev-haskell/cabal
   2015-06-24 21:47:36 (55 sec)
   2015-06-24 03:40:14 (52 sec)
  Average: 53 sec
~~~


### Category search

The search for a whole package category is indicated by a leading slash:

~~~
$ glop dev-haskell/
dev-haskell/cabal
   2015-06-24 21:47:36 (55 sec)
   2015-06-24 03:40:14 (52 sec)
  Average: 53 sec
dev-haskell/deepseq
   2015-06-24 03:41:23 (4 sec)
  Average: 4 sec
dev-haskell/http
   2015-06-24 21:52:29 (8 sec)
  Average: 8 sec
dev-haskell/cabal-install
   2015-06-24 21:52:37 (17 sec)
  Average: 17 sec
~~~


### Current emerge

You may also retrieve information about an ongoing emerge process with the `-c`
switch:

~~~
$ glop -c
dev-haskell/cabal
  running: 20 sec
  ETA:     33 sec
~~~


### Sync operations

The chronological list of portage *rsync* operations may be retrieved with `-s`:

~~~
$ glop -s
2015-07-05 17:23:21:
  rsync://91.186.30.235/gentoo-portage
  21 sec
2015-07-10 08:31:23:
  rsync://81.91.253.252/gentoo-portage
  14 sec
~~~


## Maintainer

*glop* is written by Gregor Uhlenheuer. You can reach me at
<kongo2002@gmail.com>.


## License

*glop* is licensed under the [Apache license][apache], Version 2.0

> Unless required by applicable law or agreed to in writing, software
> distributed under the License is distributed on an "AS IS" BASIS,
> WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
> See the License for the specific language governing permissions and
> limitations under the License.


[gentoo]: https://www.gentoo.org/
[apache]: http://www.apache.org/licenses/LICENSE-2.0
