use std::{
    mem::{self, MaybeUninit},
    ptr::{self, NonNull},
    cmp::{Ordering},
    slice::{self},
};

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct DbAddress {
    ptr: NonNull<u32>
}
impl DbAddress {
    #[inline(always)]
    fn new(ptr: *mut u32) -> DbAddress {
        DbAddress { ptr: unsafe { NonNull::new_unchecked(ptr) } }
    }
    #[inline(always)]
    fn get(self) -> *mut u32 {
        self.ptr.as_ptr()
    }
}

pub struct Database {
    ptrs: Vec<*mut u32>,
    pools: [*mut u32; Database::NumberOfBuckets],
    free: usize,
}
impl Database {
    const StorageSize: usize = Database::MaximumRecordSize << 8;
    const MaximumRecordSize: usize = 1usize << Database::NumberOfBuckets;
    const MaximumAllocableSliceSize: usize = Database::MaximumRecordSize - 2usize;
    const NumberOfBuckets: usize = 20usize;
    const MaximumBucket: usize = Database::NumberOfBuckets - 1usize;
    const AllocationThreshold: usize = Database::MaximumRecordSize << 4;
    pub fn new() -> Database {
        Database {
            ptrs: Vec::new(),
            pools: [ptr::null_mut(); Database::NumberOfBuckets],
            free: 0usize,
        }
    }
    pub fn allocate32(&mut self, slice: &[u32]) -> Option<DbAddress> {
        if slice.len() < Database::MaximumAllocableSliceSize {
            let record_size = (slice.len() + 2usize) & !1usize;
            let mut ptr = self.allocate_record(record_size);
            let addr = DbAddress::new(ptr);
            unsafe { *ptr = slice.len() as u32 };
            ptr = unsafe { ptr.add(1usize) };
            for &n in slice {
                unsafe { *ptr = n; }
                ptr = unsafe { ptr.add(1usize) };
            }
            Some(addr)
        } else {
            None
        }
    }
    pub fn allocate64(&mut self, slice: &[u64]) -> Option<DbAddress> {
        let length = slice.len() << 1;
        if length < Database::MaximumAllocableSliceSize {
            let record_size = (length + 3usize) & !1usize;
            let mut ptr = self.allocate_record(record_size);
            let addr = DbAddress::new(ptr);
            unsafe { *ptr = length as u32 };
            ptr = unsafe { ptr.add(2usize) };
            for &n in slice {
                unsafe { *mem::transmute::<*mut u32, *mut u64>(ptr) = n; }
                ptr = unsafe { ptr.add(2usize) };
            }
            Some(addr)
        } else {
            None
        }
    }
    pub fn allocate128(&mut self, slice: &[u128]) -> Option<DbAddress> {
        let length = slice.len() << 2;
        if length < Database::MaximumAllocableSliceSize {
            let record_size = (length + 5usize) & !1usize;
            let mut ptr = self.allocate_record(record_size);
            let addr = DbAddress::new(ptr);
            unsafe { *ptr = length as u32 };
            ptr = ((ptr as usize + 16usize) & !15usize) as *mut u32;
            for &n in slice {
                unsafe { *mem::transmute::<*mut u32, *mut u128>(ptr) = n; }
                ptr = unsafe { ptr.add(4usize) };
            }
            Some(addr)
        } else {
            None
        }
    }
    pub fn deallocate32(&mut self, addr: DbAddress) {
        let start = addr.get();
        let length = unsafe { *start as usize };
        let record_size = (length + 2usize) & !1usize;
        let bucket = unsafe { Database::size_to_bucket(record_size) };
        Database::split_in_buckets(&mut self.pools, start, record_size, bucket);
        self.free += record_size;
    }
    pub fn deallocate64(&mut self, addr: DbAddress) {
        let start = addr.get();
        let length = unsafe { *start as usize };
        let record_size = (length + 3usize) & !1usize;
        let bucket = unsafe { Database::size_to_bucket(record_size) };
        Database::split_in_buckets(&mut self.pools, start, record_size, bucket);
        self.free += record_size;
    }
    pub fn deallocate128(&mut self, addr: DbAddress) {
        let start = addr.get();
        let length = unsafe { *start as usize };
        let record_size = (length + 5usize) & !1usize;
        let bucket = unsafe { Database::size_to_bucket(record_size) };
        Database::split_in_buckets(&mut self.pools, start, record_size, bucket);
        self.free += record_size;
    }
    pub fn retrieve32(&self, addr: DbAddress) -> &[u32] {
        let length = unsafe { *addr.get() as usize};
        unsafe { slice::from_raw_parts(addr.get().add(1usize), length) }
    }
    pub fn retrieve64(&self, addr: DbAddress) -> &[u64] {
        let length = unsafe { ((*addr.get()) >> 1) as usize };
        let ptr64 = unsafe { mem::transmute::<*mut u32, *mut u64>(addr.get().add(2usize)) };
        unsafe { slice::from_raw_parts(ptr64, length) }
    }
    pub fn retrieve128(&self, addr: DbAddress) -> &[u128] {
        let length = unsafe { ((*addr.get()) >> 2) as usize };
        let ptr128 = ((addr.get() as usize + 16usize) & !15usize) as *mut u128;
        unsafe { slice::from_raw_parts(ptr128, length) }
    }
    fn allocate_record(&mut self, record_size: usize) -> *mut u32 {
        let bucket = if let Some(bucket) = self.allocate_record_from_pools(record_size) {
            bucket
        } else {
            self.restructure(record_size)
        };
        let ptr = self.pools[bucket];
        self.pools[bucket] = unsafe { *Database::as_ptr(ptr) };
        unsafe { Database::split_in_buckets(&mut self.pools, ptr.add(record_size), (1usize << (bucket + 1)) - record_size, bucket); }
        self.free -= record_size;
        ptr
    }
    fn allocate_record_from_pools(&self, record_size: usize) -> Option<usize> {
        let mut bucket = unsafe { Database::size_to_bucket(record_size) };
        while bucket < Database::NumberOfBuckets {
            if !self.pools[bucket].is_null() {
                return Some(bucket);
            }
            bucket += 1usize;
        }
        None
    }
    fn restructure(&mut self, record_size: usize) -> usize {
        let bucket_opt = if self.free >= Database::AllocationThreshold {
            let mut dfg = Defragmentator::new(self);
            if dfg.defragment() >= self.free / 2 {
                None
            } else {
                self.allocate_record_from_pools(record_size)
            }
        } else {
            None
        };
        if let Some(bucket) = bucket_opt {
            bucket
        } else {
            self.storage();
            Database::MaximumBucket
        }
    }
    fn storage(&mut self) {
        let mut vec = Vec::<u64>::with_capacity(Database::StorageSize >> 1);
        unsafe { vec.set_len(Database::StorageSize >> 1); }
        let bx_slice: Box<[u64]> = vec.into_boxed_slice();
        let ptr_slice: *mut [u64] = Box::into_raw(bx_slice);
        let ptr64: *mut u64 = unsafe { (*ptr_slice).as_mut_ptr() };
        let ptr = unsafe { mem::transmute::<*mut u64, *mut u32>(ptr64) };
        self.ptrs.push(ptr);
        self.ptrs.sort();
        let mut chunk_ptr = ptr;
        let end = unsafe { ptr.add(Database::StorageSize) };
        while chunk_ptr != end {
            let next_ptr = unsafe { chunk_ptr.add(Database::MaximumRecordSize) };
            let link = if next_ptr != end {
                next_ptr
            } else {
                self.pools[Database::MaximumBucket]
            };
            unsafe { *Database::as_ptr(chunk_ptr) = link; }
            chunk_ptr = next_ptr;
        }
        self.pools[Database::MaximumBucket] = ptr;
        self.free += Database::StorageSize;
    }
    fn split_in_buckets(pools: &mut [*mut u32; Database::NumberOfBuckets], ptr: *mut u32, size: usize, bucket: usize) {
        if size != 0usize {
            let mut curr_ptr = ptr;
            let mut curr_bucket = usize::min(bucket, Database::MaximumBucket);
            if curr_bucket == Database::MaximumBucket {
                let mut num_max = size >> (curr_bucket + 1);
                while num_max > 0usize {
                    curr_ptr = unsafe { Database::free_chunk(pools, curr_ptr, curr_bucket, Database::MaximumRecordSize) };
                    num_max -= 1usize;
                }
                curr_bucket -= 1usize;
            }
            let mut curr_mask = 1usize << (curr_bucket + 1);
            while curr_mask > 1usize {
                if size & curr_mask != 0usize {
                    curr_ptr = unsafe { Database::free_chunk(pools, curr_ptr, curr_bucket, curr_mask) };
                }
                curr_mask >>= 1;
                curr_bucket = curr_bucket.wrapping_sub(1usize);
            }
        }
    }
    #[inline(always)]
    unsafe fn as_ptr(ptr: *mut u32) -> *mut *mut u32 {
        mem::transmute::<*mut u32, *mut *mut u32>(ptr)
    }
    #[inline(always)]
    unsafe fn free_chunk(pools: &mut [*mut u32; Database::NumberOfBuckets], ptr: *mut u32, bucket: usize, size: usize) -> *mut u32 {
        let pool = &mut pools[bucket];
        *Database::as_ptr(ptr) = *pool;
        *pool = ptr;
        ptr.add(size)
    }
    #[inline(always)]
    unsafe fn size_to_bucket (size: usize) -> usize {
        (8u32 * mem::size_of::<usize>() as u32)
            .checked_sub((size - 1usize).leading_zeros() + 1u32)
            .map_or_else(|| Database::MaximumBucket, |x| x as usize)
    }
}
impl Drop for Database {
    fn drop(&mut self) {
        for &ptr in &self.ptrs {
            unsafe { Vec::<u32>::from_raw_parts(ptr, Database::StorageSize, Database::StorageSize); }
        }
    }
}

pub struct DefragmentatorItem {
    ptr: *mut u32,
    bucket: u8,
}
impl DefragmentatorItem {
    #[inline(always)]
    fn new(ptr: *mut u32, bucket: u8) -> DefragmentatorItem {
        DefragmentatorItem { ptr: ptr, bucket: bucket }
    }
    #[inline(always)]
    fn pointer(&self) -> *mut u32 {
        self.ptr
    }
    #[inline(always)]
    fn size(&self) -> usize {
        1usize << (self.bucket + 1u8)
    }
    #[inline(always)]
    fn bucket(&self) -> usize {
        self.bucket as usize
    }
}
impl PartialEq for DefragmentatorItem {
    #[inline(always)]
    fn eq(&self, other: &DefragmentatorItem) -> bool {
        self.ptr == other.ptr
    }
}
impl Eq for DefragmentatorItem {}
impl PartialOrd for DefragmentatorItem {
    #[inline(always)]
    fn partial_cmp(&self, other: &DefragmentatorItem) -> Option<Ordering> {
        self.ptr.partial_cmp(&other.ptr)
    }
}
impl Ord for DefragmentatorItem {
    #[inline(always)]
    fn cmp(&self, other: &DefragmentatorItem) -> Ordering {
        self.ptr.cmp(&other.ptr)
    }
}

pub struct Defragmentator<'a> {
    pools: [Vec<*mut u32>; Database::NumberOfBuckets],
    counts: [u8; Database::NumberOfBuckets],
    buffer: Vec<DefragmentatorItem>,
    db: &'a mut Database,
}
impl<'a> Defragmentator<'a> {
    const MaxFetch: usize = u8::max_value() as usize;
    fn new(db: &mut Database) -> Defragmentator {
        let mut vpools = {
            let mut data: [MaybeUninit<Vec<*mut u32>>; Database::NumberOfBuckets] =
                unsafe { MaybeUninit::uninit().assume_init() };
            for elem in &mut data[..] {
                *elem = MaybeUninit::new(Vec::new());
            }
            unsafe { mem::transmute::<
                [MaybeUninit<Vec<*mut u32>>; Database::NumberOfBuckets],
                [Vec<*mut u32>; Database::NumberOfBuckets]>(data)
            }
        };
        let mut counts = [0u8; Database::NumberOfBuckets];
        let mut buffer: Vec<DefragmentatorItem> = Vec::new();
        for bucket in 0 .. Database::NumberOfBuckets {
            let mut ptr = db.pools[bucket];
            db.pools[bucket] = ptr::null_mut();
            let vpool = &mut vpools[bucket];
            while !ptr.is_null() {
                vpool.push(ptr);
                ptr = unsafe { *Database::as_ptr(ptr) };
            }
            vpool.sort_by(|x, y| x.cmp(y).reverse());
            let len = vpool.len();
            let fetch = usize::min(len, Defragmentator::MaxFetch);
            counts[bucket] = fetch as u8;
            for &ptr in &vpool[(len - fetch) .. len] {
                buffer.push(DefragmentatorItem::new(ptr, bucket as u8));
            }
            vpool.resize(len - fetch, ptr::null_mut());
        }
        buffer.sort_by(|x, y| x.cmp(y).reverse());
        Defragmentator {
            pools: vpools,
            counts: counts,
            buffer: buffer,
            db: db,
        }
    }
    fn defragment(&mut self) -> usize {
        let mut ptrs_it = self.db.ptrs.iter();
        let mut stg_end: *mut u32 = ptr::null_mut();
        let mut gap_start: *mut u32 = ptr::null_mut();
        let mut gap_size = 0usize;
        let mut gap_end = gap_start;
        let mut unmergeable = 0usize;
        let mut merged = false;
        while let Some(item) = self.buffer.pop() {
            let bucket = item.bucket();
            let count = &mut self.counts[bucket];
            *count -= 1u8;
            if *count == 0u8 {
                let pool = &mut self.pools[bucket];
                let len = pool.len();
                let fetch = usize::min(len, Defragmentator::MaxFetch);
                *count = fetch as u8;
                for &ptr in &pool[(len - fetch) .. len] {
                    self.buffer.push(DefragmentatorItem::new(ptr, bucket as u8));
                }
                pool.resize(len - fetch, ptr::null_mut());
                self.buffer.sort_by(|x, y| x.cmp(y).reverse());
            }
            let next_ptr = item.pointer();
            let size = item.size();
            merged |= size & gap_size != 0usize;
            if next_ptr != gap_end || next_ptr >= stg_end {
                Defragmentator::merge(&mut self.db.pools, gap_start, gap_size);
                if !merged {
                    unmergeable += gap_size;
                }
                merged = false;
                gap_start = next_ptr;
                gap_size = 0;
                gap_end = next_ptr;
            }
            while next_ptr >= stg_end {
                stg_end = unsafe { (ptrs_it.next().unwrap()).add(Database::StorageSize) };
            }
            gap_end = unsafe { gap_end.add(size) };
            gap_size += size;
        }
        Defragmentator::merge(&mut self.db.pools, gap_start, gap_size);
        unmergeable
    }
    #[inline]
    fn merge(pools: &mut [*mut u32; Database::NumberOfBuckets], ptr: *mut u32, size: usize) {
        if size > 0usize {
            let bucket = unsafe { usize::min(Database::MaximumBucket, Database::size_to_bucket(size)) };
            Database::split_in_buckets(pools, ptr, size, bucket);
        }
    }
}

#[cfg(test)]
pub mod test {
    use std::ptr::{NonNull};
    use rand::{self, Rng};
    use crate::{
        database::{DbAddress, Database},
    };

	fn generate_record32<R: Rng>(rng: &mut R, maxlength: usize) -> Vec<u32> {
        let length = rng.gen_range(0usize, maxlength);
        let mut vec = Vec::new();
        vec.reserve(length);
        for _ in 0 .. length {
            vec.push(rng.gen());
        }
        vec
    }
	fn generate_record64<R: Rng>(rng: &mut R, maxlength: usize) -> Vec<u64> {
        let length = rng.gen_range(0usize, maxlength);
        let mut vec = Vec::new();
        vec.reserve(length);
        for _ in 0 .. length {
            vec.push(rng.gen());
        }
        vec
    }
	fn generate_record128<R: Rng>(rng: &mut R, maxlength: usize) -> Vec<u128> {
        let length = rng.gen_range(0usize, maxlength);
        let mut vec = Vec::new();
        vec.reserve(length);
        for _ in 0 .. length {
            vec.push(rng.gen());
        }
        vec
    }

    enum Record {
        U32(Vec<u32>),
        U64(Vec<u64>),
        U128(Vec<u128>),
    }
    impl Record {
        fn generate<R: Rng>(rng: &mut R) -> Record {
            let bucket = rng.gen_range(0usize, Database::NumberOfBuckets);
            if rng.gen() {
                let maxlength = usize::min(Database::MaximumAllocableSliceSize, 1usize << (bucket + 1usize));
                Record::U32(generate_record32(rng, maxlength))
            } else if rng.gen() {
                let maxlength = usize::min(Database::MaximumAllocableSliceSize >> 1, 1usize << (bucket + 1usize));
                Record::U64(generate_record64(rng, maxlength))
            } else {
                let maxlength = usize::min(Database::MaximumAllocableSliceSize >> 2, 1usize << (bucket + 1usize));
                Record::U128(generate_record128(rng, maxlength))
            }
        }
    }

    #[test]
    fn test_database() {
        let mut rng = rand::thread_rng();
        let mut db = Database::new();
        let mut old = Vec::<(DbAddress, Record)>::new();
        for _ in 0 .. 10 {
            let mut records = Vec::<(DbAddress, Record, bool)>::new();
            for _ in 0 .. 1000 {
                let record = Record::generate(&mut rng);
                let del: bool = rng.gen();
                let addr = DbAddress { ptr: NonNull::dangling() };
                records.push((addr, record, del))
            }
            for (addr, record, _) in &mut records {
                let opt_addr = match &record {
                    Record::U32(v) => db.allocate32(v),
                    Record::U64(v) => db.allocate64(v),
                    Record::U128(v) => db.allocate128(v),
                };
                *addr = opt_addr.unwrap();
            }
            while let Some((addr, record, del)) = records.pop() {
                if del {
                    match record {
                        Record::U32(_) => db.deallocate32(addr),
                        Record::U64(_) => db.deallocate64(addr),
                        Record::U128(_) => db.deallocate128(addr),
                    }
                } else {
                    old.push((addr, record));
                }
            }
            let mut size = 0usize;
            for (addr, record) in &old {
                match record {
                    Record::U32(v) => {
                        assert!(&v[..] == db.retrieve32(*addr));
                        size += (v.len() + 2usize) & !1usize;
                    },
                    Record::U64(v) => {
                        assert!(&v[..] == db.retrieve64(*addr));
                        size += ((v.len() << 1) + 3usize) & !1usize
                    },
                    Record::U128(v) => {
                        assert!(&v[..] == db.retrieve128(*addr));
                        size += ((v.len() << 2) + 5usize) & !1usize
                    },
                }
            }
            for bucket in 0 .. Database::NumberOfBuckets {
                let mut count = 0usize;
                let mut ptr = db.pools[bucket];
                while !ptr.is_null() {
                    count += 1usize;
                    ptr = unsafe { *Database::as_ptr(ptr) };
                }
                size += (1 << (bucket + 1)) * count;
            }
            assert!(size == Database::StorageSize * db.ptrs.len());
        }
    }
}