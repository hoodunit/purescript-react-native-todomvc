'use strict';

// module Main

exports.unsafeLog = function(data){
  return function(){
    console.log(data);
  }
}

exports.unsafeLog2 = function(data){
  console.log(data);
  return data;
}
