
echo Making release out of directory $PWD
make clean

TMPDIR=sage-src

# We now longer really hack any of the repos contents, but 
mkdir $TMPDIR
cp -r .depend `ls | grep -v '_darcs' | grep -v 'Simplify$'` $TMPDIR/

tar czf sage.tar.gz $TMPDIR
zip sage.zip -r $TMPDIR
rm -rf $TMPDIR
