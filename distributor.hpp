#ifndef DISTRIBUTOR_HPP
#define DISTRIBUTOR_HPP

#include <condition_variable>
#include <mutex>
#include <queue>
#include <stdexcept>
#include <thread>
#include <vector>

template <typename Type, typename Queue = std::queue<Type>>
class distributor: Queue, std::mutex, std::condition_variable {
  typename Queue::size_type capacity;
  bool done = false;
  std::vector<std::thread> threads;
  using Queue::emplace;
  using Queue::empty;
  using Queue::front;
  using Queue::pop;
  using Queue::size;

public:
  template<typename Function>
  distributor( Function function
             , unsigned int concurrency = std::thread::hardware_concurrency()
	     , typename Queue::size_type max_items_per_thread = 1
	     )
  : capacity{concurrency * max_items_per_thread}
  {
    if (not concurrency)
      throw std::invalid_argument("Concurrency must be non-zero");
    if (not max_items_per_thread)
      throw std::invalid_argument("Max items per thread must be non-zero");

    for (unsigned int count {0}; count < concurrency; count += 1)
      threads.emplace_back(static_cast<void (distributor::*)(Function)>
                           (&distributor::consume), this, function);
  }

  // disable move
  distributor(distributor &&) = delete;
  distributor &operator=(distributor &&) = delete;

  template<typename... Args>
  void operator()(Args&&... args) {
    std::unique_lock<std::mutex> lock(*this);
    while (size() == capacity) wait(lock);
    emplace(std::forward<Args>(args)...);
    notify_one();
  }

  ~distributor() {
    {
      std::lock_guard<std::mutex> guard(*this);
      done = true;
      notify_all();
    }
    for (auto &&thread: threads) thread.join();
  }

private:
  template <typename Function>
  void consume(Function process) {
    std::unique_lock<std::mutex> lock(*this);
    while (true) {
      if (not empty()) {
        Type item { std::move(front()) };
        pop();
        notify_one();
        lock.unlock();
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
