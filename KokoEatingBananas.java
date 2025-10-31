class Solution {
    // hàm này để checkxem 1 số có thỏa mãn điều kiện đề bài hay không 
    public boolean check(int[] a , int k,int h){
        int sum = 0;
        for(int i= 0; i < a.length;i++){ // a.lenght = n thì O(n)
            if( a[i] % k == 0){
                sum = sum + a[i]/k;
            }
            else{
                sum =  sum + a[i]/k +1;
            }
        }
        if (sum > h){
            return false;
        }
        else {
            return true;
        }
    }
    public int minEatingSpeed(int[] piles, int h) {
        int n = piles.length;
        int max = 0;
        // tìm max 
        for (int i =0; i < n ;i++){ // O(n)

            if(max < piles[i]){
                max=piles[i];
            }
        }
        if (h < n){
            return max;
        }
        // dùng binary sreach tìm xem từ số 1 đến max số nhỏ nhất thỏa mãn điều kiện đề bài
        int dau = 1;
        int cuoi = max;
        int  min = max;
        while (dau <= cuoi){
            int giua = dau + ((cuoi - dau))/2;
            if (check(piles,giua,h)==  true){
                min = giua;
                cuoi = giua -1;
            }else if (check(piles,giua,h) == false ){
                dau = giua +1;
            }
        }
        return min;
    }
}
// độ phức tạp thuật toán O(N * log M) với N là piles.length và M là max(piles)
