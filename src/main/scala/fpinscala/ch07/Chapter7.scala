package fpinscala.ch07

object Chapter7 {
    def sum(ints: IndexedSeq[Int]): Int = {
        if (ints.size <= 1) {
            ints.headOption.getOrElse(0)
        } else {
            val (l, r) = ints.splitAt(ints.length / 2)
            sum(l) + sum(r)
        }
    }

    /** def sum_par1(ints: IndexedSeq[Int]): Int = {
        if (ints.size <= 1) {
            ints.headOption.getOrElse(0)
        } else {
            val (l, r) = ints.splitAt(ints.length / 2)
            // if unit immediatelly starts computation in different
            // thread, than sum is not parallel because get(sumL)
            // blocks thread until sumL finishes and than
            // blocks until get(sumR) finishes.
            //
            // we would like to start our computation and
            // wait only for final result.
            // To do this, we need way to combine two Par's
            // into new one in non-blocking way.
            val sumL = Par.unit(sum_par1(l))
            val sumR = Par.unit(sum_par1(r))
            Par.run(sumL) + Par.run(sumR)
        }
    } */

    def sum_par2(ints: IndexedSeq[Int]): Par[Int] = {
        if (ints.size <= 1) {
            // This is fairly inefficient, because
            // unit spawns new parallel computation in
            // new thread which is unneccessary.
            // We should consider way to create new Par
            // that doesn't start new thread
            Par.unit(ints.headOption.getOrElse(0))
        } else {
            val (l, r) = ints.splitAt(ints.length / 2)
            Par.map2(sum_par2(l), sum_par2(r))(_ +  _)
        }
    }

    /*
     * If map2 params are strictly evaluated,
     * xs = List(1, 2, 3, 4)
     * 0:
     *   (l, r) = (List(1, 2), List(3, 4))
     *   map2(sum(List(1, 2)), sum(List(3, 4)))(_ + _)
     * 1a:
     *   (l, r) = ([1], [2])
     *   map2(sum([1]), sum([2]))(_ + _)
     *   map2 is strict which means Pars for left splits are created and run
     *   before right splits
     * 2a:
     *  unit([1].getOrElse(0)) -> computes value before its wrapped in new thread
     *   because unit is not lazy anymore and params, are evaluated before function body
     * 2b:
     *  ...
     * 1b:
     *  (l, r) = ([3], [4])
     *  map2(sum([3]), sum([4]))(_ + _)
     * 2c:
     *  ...
     * 3d:
     *  ...
     */
}