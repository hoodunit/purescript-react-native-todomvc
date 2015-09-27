'use strict';

// module Main

exports.unsafeLog = function(data){
  return function(){
    console.log(data);
  }
}
