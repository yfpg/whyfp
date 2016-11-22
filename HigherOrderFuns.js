var arr = [1,2,3,4]

var ds = arr.map(function(a){
  return a*2
})

console.log(ds)

var foldl = function (f, z, xs){
  if (xs.length == 0){
       return z;
    } else {
      return foldl(f, f(z, xs[0]), xs.slice(1));
   }
}

function foldr(fn, ult, xs) {
    if (xs.length == 0){
        return ult;
    }else{
        return fn(xs[0], foldr(fn, ult, xs.slice(1)));
    }
}
var addF = function(a,b) {
  return a+b;
}
var s1 = foldl(addF, 0, [1,2,5,3,4]);
var s2 = foldl(addF, 0, [1,2,5,3,4]);

var left = foldl((x,y)=>'('+x+'+'+y+')', 0, [1,2,5,3,4]);
var right = foldr((x,y)=>'('+x+'+'+y+')', 0, [1,2,5,3,4]);

console.log(left)
console.log(right)
