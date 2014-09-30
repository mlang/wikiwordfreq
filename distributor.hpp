#ifndef DISTRIBUTOR_HPP
#define DISTRIBUTOR_HPP

#include <algorithm>
#include <condition_variable>
#include <mutex>
#include <queue>
#include <thread>
#include <vector>

template <typename Type, typename Queue = std::queue<Type>>
class distributor: Queue, std::mutex, std::condition_variable {
  typename Queue::size_type capacity;
  bool done = false;
  std::vector<std::thread> threads;

public:
  template<typename Function>
  distributor( Function function
             , unsigned int concurrency = std::thread::hardware_concurrency()
	     , typename Queue::size_type max_items_per_thread = 256
	     )
  : capacity{concurrency * max_items_per_thread}
  {
    for (unsigned int count {0}; count < concurrency; count += 1) {
      threads.emplace_back(static_cast<void (distributor::*)(Function)>
                           (&distributor::consume), this, function);
    }
  }

  distributor(distributor &&) = default;
  distributor(distributor const &) = delete;
  distributor& operator=(distributor const &) = delete;

  ~distributor() {
    {
      std::lock_guard<std::mutex> guard(*this);
      done = true;
    }
    notify_all();
    std::for_each(threads.begin(), threads.end(),
                  std::mem_fun_ref(&std::thread::join));
  }

  void operator()(Type &&value) {
    std::unique_lock<std::mutex> lock(*this);
    while (Queue::size() < capacity) wait(lock);
    Queue::push(std::forward<Type>(value));
    notify_one();
  }

private:
  template <typename Function>
  void consume(Function process) {
    std::unique_lock<std::mutex> lock(*this);
    while (true) {
      if (not Queue::empty()) {
        Type item { std::move(Queue::front()) };
        Queue::pop();
        lock.unlock();
        notify_one();
        process(item);
        lock.lock();
      } else if (done) {
        break;
      } else {
        wait(lock);
      }
    }
  }
};

#endif
