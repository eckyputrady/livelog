module.exports = {
  trace
};

function trace (prefix) {
  return (x) => {
    console.log(prefix, x);
    return x;
  };
}