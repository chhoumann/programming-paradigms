if [ -z "$1" ]; then
    echo "Usage: mkses.sh <lecture number>"
    exit 1
fi

# Create the directory
dir="session$1"
mkdir $dir

# Copy the template files
echo "module Session$1Spec where
    
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import SolutionSession$1

main :: IO()
main = hspec $ do
    describe \"Session$1.placeholder\" $ do\n" > $dir/test.hs

echo "module SolutionSession$1 where\n" > $dir/SolutionSession$1.hs