The accelerate darcs repository can be checked out here in the working
copy, mixing 

Unfortunately, darcs is finicky and won't let you check out to an
existing directory directly.  Below is a series of commands to
accomplish the checkout:

  darcs get http://code.haskell.org/accelerate 
  (cd accelerate; tar czvf accelerate.tgz *)
  tar xzvf accelerate/accelerate.tgz
  rm -rf accelerate/
  

In this git repository howover, we will only track the files relevant
to our ArBB experiment. 
