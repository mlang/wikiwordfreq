#ifndef DISTRIBUTOR_HPP
#define DISTRIBUTOR_HPP

#include <condition_variable>
#include <mutex>
#include <queue>
#include <stdexcept>
#include <thread>
#include <vector>

template <typename Type, typename Queue = std::queue<Type>>
class distributor: std::mutex, std::condition_variable
{
  bool done = false;
  Queue queue;
  struct : std::vector<std::thread> {
    void join() { for_each(begin(), end(), mem_fun_ref(&value_type::join)); }
  } threads;

  using lock_guard = std::lock_guard<std::mutex>;
  using unique_lock = std::unique_lock<std::mutex>;

public:
  template<typename Function>
  distributor( Function&& function
             , unsigned int concurrency = std::thread::hardware_concurrency()
             )
  {
    if (not concurrency)
      throw std::invalid_argument("Concurrency must not be zero");

    for (unsigned int count {}; count < concurrency; ++count)
      threads.emplace_back(static_cast<void (distributor::*)(Function)>
                           (&distributor::consume), this,
                           std::forward<Function>(function));
  }

  // disable move
  distributor(distributor &&) = delete;
  distributor &operator=(distributor &&) = delete;

  template<typename... Args> distributor& operator()(Args&&... args)
  {
    unique_lock lock { *this };
    while (queue.size() == threads.size()) wait(lock);
    queue.emplace(std::forward<Args>(args)...);
    notify_one();
  }

  ~distributor()
  {
    lock();
    done = true;
    notify_all();
    unlock();
    threads.join();
  }

private:
  template <typename Function>
  void consume(Function process)
  {
    unique_lock lock { *this };
    while (true) {
      if (not queue.empty()) {
        Type item { std::move(queue.front()) };
        queue.pop();
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
