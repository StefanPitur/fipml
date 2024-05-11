import std/num/int32
import std/os/env

type tree<a>
  Tip(x : a)
  Bin(l : tree<a>, r : tree<a>)

type tzipper<a,b>
  Top
  BinL(z : tzipper<a,b>, r : tree<a>)
  BinR(l : tree<b>, z : tzipper<a,b>)

fip fun down(t : tree<a>, ^f : a -> div b, z : tzipper<a,b>) : div tree<b>
  match t
    Bin(l, r) -> down (l, f, BinL (z, r))
    Tip(x) ->
      val fx = f(x)
      app (Tip(fx), f, z)

fip fun app (t : tree<b>, ^f : a -> div b, z : tzipper<a,b>) : div tree<b>
  match z
    Top -> t
    BinR(l, up) -> app (Bin(l, t), f, up)
    BinL(up, r) -> down (r, f, BinR (t, up))

fip fun tmap (t : tree<a>, ^f : a -> div b) : div tree<b>
  down (t, f, Top)

fun create_int_tree (level : int32) : div tree<int32>
  if level == 1.int32 then Tip (1.int32)
  else
    val pred_level = level - 1.int32
    val res_l = create_int_tree (pred_level)
    val res_r = create_int_tree (pred_level)
    Bin (res_l, res_r)

fun fold_sum (t : tree<int32>) : int32
  match t
    Tip(x) -> x
    Bin(l, r) ->
      val res_l = fold_sum (l)
      val res_r = fold_sum (r)
      res_l + res_r

fun main()
  val t = create_int_tree (30.int32)
  val t2 = tmap (t, fn(x) 0.int32 - x)
  fold_sum (t2).show
